/**
 * @file snake_modern.cpp
 * @brief Modern Snake game implementation using C++ STL forward_list
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
#include <list>
#include <random>
#include <iostream>
#include <iomanip>
#include <chrono>
#include <thread>

const int game_pause = 100;
const int game_frame_width = 40;
const int game_frame_height = 24;

struct terminal_state {
#ifdef _WIN32
	HANDLE input;
	HANDLE output;
	DWORD original_input_mode;
	DWORD original_output_mode;
#else
	struct termios original_termios;
#endif
	int is_raw;
};

void terminal_state_init(terminal_state* state) {
	if (state == NULL)
		return;
	
#ifdef _WIN32
	state->input = INVALID_HANDLE_VALUE;
	state->output = INVALID_HANDLE_VALUE;
	state->original_input_mode = 0;
	state->original_output_mode = 0;
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
		std::cerr << "Error: GetStdHandle(STD_INPUT_HANDLE) failed" << std::endl;
		return;
	}

	state->output = GetStdHandle(STD_OUTPUT_HANDLE);
	if (state->output == INVALID_HANDLE_VALUE) {
		std::cerr << "Error: GetStdHandle(STD_OUTPUT_HANDLE) failed" << std::endl;
		return;
	}

	// Save the original mode
	if (!GetConsoleMode(state->input, &state->original_input_mode)) {
		std::cerr << "Error: GetConsoleMode failed for stdin" << std::endl;
		return;
	}

	DWORD mode = state->original_input_mode;

	// Key settings for VT input:
	mode |= ENABLE_VIRTUAL_TERMINAL_INPUT;  // Enable VT sequences
	mode &= ~ENABLE_PROCESSED_INPUT;        // Get "raw" escape codes
	mode &= ~ENABLE_LINE_INPUT;             // Non-canonical mode
	mode &= ~ENABLE_ECHO_INPUT;             // No echo

	// Optional for mouse
	// mode |= ENABLE_MOUSE_INPUT;

	if (!SetConsoleMode(state->input, mode)) {
		// Try without ENABLE_VIRTUAL_TERMINAL_INPUT (for older Windows)
		//mode &= ~ENABLE_VIRTUAL_TERMINAL_INPUT;
		//if (!SetConsoleMode(state->input, mode)) {
		//	fprintf(stderr, "Error: SetConsoleMode failed\n");
			return;
		//}
	}

	if (!GetConsoleMode(state->output, &state->original_output_mode)) {
		std::cerr << "Error: GetConsoleMode failed for stdout" << std::endl;
		return;
	}

	mode = state->original_output_mode;
	mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
	if (!SetConsoleMode(state->output, mode))
		return;
#else
	// Save current settings
	if (tcgetattr(STDIN_FILENO, &state->original_termios) == -1) {
		std::cerr << "tcgetattr failed" << std::endl;
		return;
	}
	
	// Set non-canonical mode
	struct termios raw = state->original_termios;
	raw.c_lflag &= ~(ICANON | ECHO);
	// Additional settings for more "raw" mode:
	raw.c_lflag &= ~ISIG;  // Disable signal handling (Ctrl+C, Ctrl+Z)
	raw.c_iflag &= ~(IXON | ICRNL); // Disable flow control and CR->NL conversion
	
	// Configure minimum number of characters and timeout
	raw.c_cc[VMIN] = 1;   // Read at least 1 character
	raw.c_cc[VTIME] = 0;  // No timeout
	
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) {
		std::cerr << "tcsetattr failed" << std::endl;
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
	
	SetConsoleMode(state->input, state->original_input_mode);

	if (state->output == INVALID_HANDLE_VALUE)
		return;

	SetConsoleMode(state->output, state->original_output_mode);
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
		STATE_NORMAL,      // Normal input
		STATE_ESC,         // ESC received
		STATE_CSI,         // CSI sequence ([)
		STATE_OSC,         // OSC sequence (])
		STATE_SS3,         // SS3 sequence (O)
		STATE_IGNORE       // Ignore the rest of the sequence
	} pst = STATE_NORMAL;
	int osc_esc = 0;

	// filter esc sequences
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
				return ch; // single ESC or unknown
			break;
		case STATE_CSI:
            // CSI sequence: "ESC [ P...P I...I F"
            // Ends with a character in the range 0x40-0x7E
            if (ch >= 0x40 && ch <= 0x7E)
                pst = STATE_NORMAL;
			break;
		case  STATE_OSC:
            // OSC sequence: "ESC ] ... BEL or ESC ] ... ESC \"
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
            // SS3 sequence: ESC O F
            // Ends with one character
            pst = STATE_NORMAL;
            break;
		case STATE_IGNORE:
            // Backup state - just wait for a printable character
            if (ch >= 32 && ch <= 126)
                pst = STATE_NORMAL;
            break;            
        default:
            pst = STATE_NORMAL;
            break;
		}
	}
}

struct point {
	int x;
	int y;
};

int random_limit(int n) {
    static std::mt19937 rng(std::random_device{}());
    std::uniform_int_distribution<int> dist(0, n);
    return dist(rng);
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
    std::this_thread::sleep_for(std::chrono::milliseconds(milliseconds));
}

enum class game_action {
	Waiting,
	GameOver,
	MoveUp,
	MoveDown,
	MoveLeft,
	MoveRight
};

struct game_state {
	game_action action;
	int frame_width;
	int frame_height;
	point fruit;
	std::list<point> snake_body;
	int score;
};

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
	if (!state->snake_body.empty()) {
		const auto head = state->snake_body.cbegin();
		goto_xy(head->x, head->y);
		fwrite(&ch, 1, 1, stdout);

		if (state->snake_body.cbegin() != state->snake_body.cend())
		{
			const auto& tail = std::prev(state->snake_body.cend());
			if (tail != head)
			{
				goto_xy(tail->x, tail->y);
				fwrite(&cl, 1, 1, stdout);
			}
		}
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
		if (state->action != game_action::MoveDown)
			state->action = game_action::MoveUp;
		break;
	case 's':
	case 'S':
		if (state->action != game_action::MoveUp)
			state->action = game_action::MoveDown;
		break;
	case 'a':
	case 'A':
		if (state->action != game_action::MoveRight)
			state->action = game_action::MoveLeft;
		break;
	case 'd':
	case 'D':
		if (state->action != game_action::MoveLeft)
			state->action = game_action::MoveRight;
		break;
	case 'q':
	case 'Q':
		state->action = game_action::GameOver;
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
		for (const auto& pt : state->snake_body) {
			if (point_equal(&state->fruit, const_cast<point*>(&pt))) {
				found = 0;
				break;
			}
		}
	}
}

void do_logic(game_state* state) {
	if (state->action == game_action::Waiting)
		return;

	point head = state->snake_body.front();

	switch(state->action) {
	case game_action::MoveUp:
		--head.y;
		break;
	case game_action::MoveDown:
		++head.y;
		break;
	case game_action::MoveLeft:
		--head.x;
		break;
	case game_action::MoveRight:
		++head.x;
		break;
	default:
		break;
	}

	// Check if head is out of bounds
	if (head.x < 2 || (head.x > state->frame_width - 1) || (head.y < 2) || (head.y > state->frame_height - 1)) {
		state->action = game_action::GameOver;
		return;
	}
	
	// Check the fruit
	int goal = point_equal(&state->fruit, &head);

	if (!goal && std::next(state->snake_body.begin()) != state->snake_body.end())
		state->snake_body.pop_back();

	state->snake_body.push_front(head);
	
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
		game_action::Waiting, 
		game_frame_width, 
		game_frame_height, 
		{ game_frame_width / 2, game_frame_height /2 }, 
		std::list<point>{ 1, get_random_point() }, 
		0
	};
	cursor_off();
	clear_screen();
	draw_frame(game.frame_width, game.frame_height);

	while(1) {
		draw(&game);
		get_input(get_char(&state), &game);
		do_logic(&game);
		if (game.action == game_action::GameOver)
			break;
		sleep_ms(game_pause);
	}

	clear_screen();
	cursor_on();
	std::cout << "Score: " << game.score << std::endl;
	restore_terminal(&state);
	return 0;
}
