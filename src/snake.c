/**
 * @file snake.c
 * @brief Snake game implementation
 * @author Konstantin A. Pankov(explorus@mail.ru)
 * @license Licensed under the MIT License, Version 2.0 (see LICENSE.txt).
 */
#ifdef _WIN32
	#include <windows.h>
#else
	#include <termios.h>
	#include <unistd.h> // STDIN_FILENO
	#include <sys/select.h> // select
	#include <sys/time.h> // timeval
#endif
#include <stdio.h> //printf, perror
#include <stdlib.h> // malloc, free

const int game_pause = 100;
const int game_frame_width = 40;
const int game_frame_height = 24;

typedef struct terminal_state {
#ifdef _WIN32
	HANDLE input;
	DWORD original_mode;
#else
	struct termios original_termios;
#endif
	int is_raw;
} terminal_state;

void terminal_state_init(terminal_state* state) {
	if (state == NULL)
		return;
	
#ifdef _WIN32
	state->input = INVALID_HANDLE_VALUE;
	state->original_mode = 0;
#else
	tcgetattr(STDIN_FILENO, &state->original_termios);
#endif
	state->is_raw = 0;
}

void init_terminal(terminal_state* state) {
	if (state == NULL || state->is_raw)
		return;
	
#ifdef _WIN32
	state->input = GetStdHandle(STD_INPUT_HANDLE);
	if (state->input == INVALID_HANDLE_VALUE) {
		fprintf(stderr, "Error: GetStdHandle failed\n");
		return;
	}

	// Сохраняем оригинальный режим
	if (!GetConsoleMode(state->input, &state->original_mode)) {
		fprintf(stderr, "Error: GetConsoleMode failed\n");
		return;
	}

	DWORD mode = state->original_mode;

	// Ключевые настройки для VT-ввода:
	mode |= ENABLE_VIRTUAL_TERMINAL_INPUT;  // Включаем VT-последовательности
	mode &= ~ENABLE_PROCESSED_INPUT;        // Получаем "сырые" escape-коды
	mode &= ~ENABLE_LINE_INPUT;             // Неканонический режим
	mode &= ~ENABLE_ECHO_INPUT;             // Без эха

	// Опционально для мыши
	// mode |= ENABLE_MOUSE_INPUT;

	if (!SetConsoleMode(state->input, mode)) {
		// Попробуем без ENABLE_VIRTUAL_TERMINAL_INPUT (для старых Windows)
		//mode &= ~ENABLE_VIRTUAL_TERMINAL_INPUT;
		//if (!SetConsoleMode(state->input, mode)) {
		//	fprintf(stderr, "Error: SetConsoleMode failed\n");
			return;
		//}
	}
#else
	// Сохраняем текущие настройки
	if (tcgetattr(STDIN_FILENO, &state->original_termios) == -1) {
		perror("tcgetattr");
		return;
	}
	
	// Устанавливаем неканонический режим
	struct termios raw = state->original_termios;
	raw.c_lflag &= ~(ICANON | ECHO);
	// Дополнительные настройки для более "сырого" режима:
	raw.c_lflag &= ~ISIG;  // Отключаем обработку сигналов (Ctrl+C, Ctrl+Z)
	raw.c_iflag &= ~(IXON | ICRNL); // Отключаем управление потоком и преобразование CR->NL
	
	// Настраиваем минимальное количество символов и таймаут
	raw.c_cc[VMIN] = 1;   // Читать хотя бы 1 символ
	raw.c_cc[VTIME] = 0;  // Без таймаута
	
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) {
		perror("tcsetattr");
		return;
	}
#endif
	state->is_raw = 1;
}

void restore_terminal(terminal_state* state) {
	if (state == NULL || !state->is_raw)
		return;

#ifdef _WIN32
	if (state->input == INVALID_HANDLE_VALUE)
		return;
	
	SetConsoleMode(state->input, state->original_mode);
#else
   
	tcsetattr(STDIN_FILENO, TCSAFLUSH, &state->original_termios);
#endif
	state->is_raw = 0;
}

void goto_xy(int x, int y) {
    printf("\033[%d;%dH", y, x);
    fflush(stdout);
}

void cursor_on(void) {
	const char esc_seq[] = "\033[?25h";
	fwrite(esc_seq, 1, sizeof(esc_seq) - 1, stdout);
	fflush(stdout);
}

void cursor_off(void) {
	const char esc_seq[] = "\033[?25l";
	fwrite(esc_seq, 1, sizeof(esc_seq) - 1, stdout);
	fflush(stdout);
}

void clear_screen(void) {
	const char esc_seq[] = "\033[2J\033[H";
	fwrite(esc_seq, 1, sizeof(esc_seq) - 1, stdout);
	fflush(stdout);
}

int terminal_read_char(terminal_state* state) {
	if (state == NULL || !state->is_raw)
		return EOF;

	char ch;
#ifdef _WIN32
	while (1) {
		INPUT_RECORD input_record;
		DWORD events_read = 0;

		if (!PeekConsoleInput(state->input, &input_record, 1, &events_read))
			return EOF;

		if (events_read == 0)
			return EOF;

		if (input_record.EventType == KEY_EVENT &&
			input_record.Event.KeyEvent.bKeyDown)
			break;

		ReadConsoleInput(state->input, &input_record, 1, &events_read);
	}

	DWORD charsRead = 0;
	if (!ReadConsoleA(state->input, &ch, 1, &charsRead, NULL))
		return EOF;
#else
	fd_set readfds;
    struct timeval timeout = {0, 0};
    
    FD_ZERO(&readfds);
    FD_SET(STDIN_FILENO, &readfds);
    
    int result = select(STDIN_FILENO + 1, &readfds, NULL, NULL, &timeout);
    
    if (result > 0 && FD_ISSET(STDIN_FILENO, &readfds) && read(STDIN_FILENO, &ch, 1) != 1)
		return EOF;
#endif
	return (unsigned char)ch;
}

int get_char(terminal_state* state) {
	enum parser_state {
		STATE_NORMAL,      // Обычный ввод
		STATE_ESC,         // Получен ESC
		STATE_CSI,         // CSI последовательность ([)
		STATE_OSC,         // OSC последовательность (])
		STATE_SS3,         // SS3 последовательность (O)
		STATE_IGNORE       // Игнорируем оставшуюся часть последовательности
	} pst = STATE_NORMAL;
	int osc_esc = 0;

	// фильтруем esc-последовательности
	while(1) {
		int ch = terminal_read_char(state);

		if (ch == EOF)
			return ch;

		switch(pst)
		{
		case STATE_NORMAL:
			if (ch == 27) // ESC
				pst = STATE_ESC;
			else
				return ch;
			break;
		case STATE_ESC:
			if (ch == '[')
				pst = STATE_CSI;
			else if (ch == ']')
			{
				pst = STATE_OSC;
				osc_esc = 0;
			}
			else if (ch == 'O')
				pst = STATE_SS3;
			else
				return ch; // одиночный ESC или непонятно что
			break;
		case STATE_CSI:
			// CSI последовательность: "ESC [ P...P I...I F"
            // Завершается символом в диапазоне 0x40-0x7E
            if (ch >= 0x40 && ch <= 0x7E)
                pst = STATE_NORMAL;
			break;
		case  STATE_OSC:
            // OSC последовательность: "ESC ] ... BEL или ESC ] ... ESC \"
            if (osc_esc) {
                if (ch == '\\')
                    pst = STATE_NORMAL;
                osc_esc = 0;
            }
			else if (ch == 27)
                osc_esc = 1;
            else if (ch == '\a') // BEL
                pst = STATE_NORMAL;
            break;
		case STATE_SS3:
            // SS3 последовательность: ESC O F
            // Завершается одним символом
            pst = STATE_NORMAL;
            break;
		case STATE_IGNORE:
            // Резервное состояние - просто ждём печатного символа
            if (ch >= 32 && ch <= 126)
                pst = STATE_NORMAL;
            break;            
        default:
            pst = STATE_NORMAL;
            break;
		}
	}
}

typedef struct point {
	int x;
	int y;
} point;

typedef struct node {
    point data;
    struct node* next;
} node;

typedef struct list {
	node* head;
} list;

void push_head(list* lst, point pt) {
	node* new_node = malloc(sizeof(node));
	new_node->data = pt;
	new_node->next = lst->head;
	lst->head = new_node;
}

void pop_tail(list* lst) {
	node* tail = lst->head;
	node* prev = NULL;

	while(tail && tail->next)
	{
		prev = tail;
		tail = tail->next;
	}

	if (prev)
		prev->next = NULL;
	else
		lst->head = NULL;

	free(tail);
}

int random_limit(int n) {
    if (n < RAND_MAX) {
        int limit = RAND_MAX - (RAND_MAX % (n + 1));
        int r;

        do {
            r = rand();
        } while (r > limit);

        return r % (n + 1);
    }
	else
        return ((long)rand() * (n + 1)) / (RAND_MAX + 1L);
}

int point_equal(point* pt1, point* pt2) {
	return pt1->x == pt2->x && pt1->y == pt2->y;	
}

point get_random_point(void) {
	point pt;
	pt.x = random_limit(game_frame_width - 6) + 3;
	pt.y = random_limit(game_frame_height - 6) + 3;
	return pt;
}

void sleep_ms(int milliseconds) {
#ifdef _WIN32
    Sleep(milliseconds);
#else
    usleep(milliseconds * 1000);
#endif
}

typedef enum game_action {
	Waiting,
	GameOver,
	MoveUp,
	MoveDown,
	MoveLeft,
	MoveRight
} game_action;

typedef struct game_state {
	game_action action;
	int frame_width;
	int frame_height;
	point fruit;
	list snake_body;
	int score;
} game_state;

void draw_frame(int width, int height) {
	const char ch = '#';
	const char space = ' ';

	goto_xy(1, 1);
	for (int column = 0; column < width; column++)
		fwrite(&ch, 1, 1, stdout);

	for (int row = 2; row < height; row++) {
		goto_xy(1, row);
		fwrite(&ch, 1, 1, stdout);
		for (int column = 1; column < width - 1; column++)
			fwrite(&space, 1, 1, stdout);
		fwrite(&ch, 1, 1, stdout);
	}

	goto_xy(1, height);
	for (int column = 0; column < width; column++)
		fwrite(&ch, 1, 1, stdout);

	fflush(stdout);
}

void draw(game_state* state) {
	const char ch = 'O';
	const char cl = ' ';
	const char fruit = 'F';

	// Draw the snake
	node* tail_node = state->snake_body.head;
	point pt = tail_node->data;
	goto_xy(pt.x, pt.y);
	fwrite(&ch, 1, 1, stdout);

	while (tail_node->next)
		tail_node = tail_node->next;
	
	if (tail_node != state->snake_body.head) {
		pt = tail_node->data;
		goto_xy(pt.x, pt.y);
		fwrite(&cl, 1, 1, stdout);
	}

	// Draw the fruit
	goto_xy(state->fruit.x, state->fruit.y);
	fwrite(&fruit, 1, 1, stdout);
	fflush(stdout);
}

void get_input(int ch, game_state* state) {
	if (ch == EOF)
		return;

	switch(ch) {
	case 'w':
	case 'W': 
		if (state->action != MoveDown)
			state->action = MoveUp;
		break;
	case 's':
	case 'S':
		if (state->action != MoveUp)
			state->action = MoveDown;
		break;
	case 'a':
	case 'A':
		if (state->action != MoveRight)
			state->action = MoveLeft;
		break;
	case 'd':
	case 'D':
		if (state->action != MoveLeft)
			state->action = MoveRight;
		break;
	case 'q':
	case 'Q':
		state->action = GameOver;
		break;
	default:
		break;
	}
}

void update_fruit(game_state* state) {
	int found = 0;

	while (!found) {
		state->fruit = get_random_point();
		found = 1;
		node* tail_node = state->snake_body.head;

		while (tail_node) {
			if (point_equal(&state->fruit, &tail_node->data)) {
				found = 0;
				break;
			}
			tail_node = tail_node->next;
		}
	}
}

void do_logic(game_state* state) {
	if (state->action == Waiting)
		return;

	point head = state->snake_body.head->data;

	switch(state->action) {
	case MoveUp:
		--head.y;
		break;
	case MoveDown:
		++head.y;
		break;
	case MoveLeft:
		--head.x;
		break;
	case MoveRight:
		++head.x;
		break;
	default:
		break;
	}

	// Check if head is out of bounds
	if (head.x < 2 || (head.x > state->frame_width - 1) || (head.y < 2) || (head.y > state->frame_height - 1)) {
		state->action = GameOver;
		return;
	}
	
	// Check the fruit
	int goal = point_equal(&state->fruit, &head);

	if (!goal && state->snake_body.head->next)
		pop_tail(&state->snake_body);

	push_head(&state->snake_body, head);
	
	if (goal) {
		state->score += 10;
		update_fruit(state);
	}
}

int main(void)
{
	terminal_state state;
	terminal_state_init(&state);
	init_terminal(&state);

	game_state game = { 
		Waiting, 
		game_frame_width, 
		game_frame_height, 
		{ game_frame_width / 2, game_frame_height /2 }, 
		{ NULL }, 
		0
	};
	push_head(&game.snake_body, get_random_point());
	cursor_off();
	clear_screen();
	draw_frame(game.frame_width, game.frame_height);

	while(1) {
		draw(&game);
		get_input(get_char(&state), &game);
		do_logic(&game);
		if (game.action == GameOver)
			break;
		sleep_ms(game_pause);
	}

	clear_screen();
	cursor_on();
	printf("Score: %d\n", game.score);
	restore_terminal(&state);
	return 0;
}