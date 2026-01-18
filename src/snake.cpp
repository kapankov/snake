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
extern "C" {
#include "tio.h"
}
const int game_pause = 100;
const int game_frame_width = 40;
const int game_frame_height = 24;

int get_char(TERM* term) {
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
		int ch = terminal_read_char(term);

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

	bool operator==(const point& other) const {
		return x == other.x && y == other.y;
	}
};

int random_limit(int n) {
    static std::mt19937 rng(std::random_device{}());
    std::uniform_int_distribution<int> dist(0, n);
    return dist(rng);
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
			if (state->fruit == pt) {
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
	int goal = state->fruit == head;

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
	TERM* term = init_terminal();

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
		get_input(get_char(term), &game);
		do_logic(&game);
		if (game.action == game_action::GameOver)
			break;
		sleep_ms(game_pause);
	}

	clear_screen();
	cursor_on();
	std::cout << "Score: " << game.score << std::endl;
	restore_terminal(term);
	return 0;
}
