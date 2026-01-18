#include "tio.h"

#ifndef _WIN32
	#include <unistd.h> // STDIN_FILENO
	#include <sys/select.h> // select
	#include <sys/time.h> // timeval
#endif
#include <stdio.h>  //printf
#include <stdlib.h> // malloc, free

struct terminal_state {
#ifdef _WIN32
	HANDLE input;
	HANDLE output;
	DWORD original_input_mode;
	DWORD original_output_mode;
#else
	struct termios original_termios;
#endif
};

static TERM* terminal_state_init(void) {
	TERM* term = malloc(sizeof(TERM));
	if (term == NULL)
		return NULL;
	
#ifdef _WIN32
	term->input = INVALID_HANDLE_VALUE;
	term->output = INVALID_HANDLE_VALUE;
	term->original_input_mode = 0;
	term->original_output_mode = 0;
#else
	tcgetattr(STDIN_FILENO, &term->original_termios);
#endif
	return term;
}

TERM* init_terminal(void) {
	TERM* term = terminal_state_init();
	if (term == NULL)
		return NULL;

#ifdef _WIN32
	term->input = GetStdHandle(STD_INPUT_HANDLE);
	if (term->input == INVALID_HANDLE_VALUE) {
		fprintf(stderr, "Error: GetStdHandle(STD_INPUT_HANDLE) failed\n");
		return NULL;
	}

	term->output = GetStdHandle(STD_OUTPUT_HANDLE);
	if (term->output == INVALID_HANDLE_VALUE) {
		fprintf(stderr, "Error: GetStdHandle(STD_OUTPUT_HANDLE) failed\n");
		return NULL;
	}

	// Save the original mode
	if (!GetConsoleMode(term->input, &term->original_input_mode)) {
		fprintf(stderr, "Error: GetConsoleMode failed for stdin\n");
		return NULL;
	}

	DWORD mode = term->original_input_mode;

	// Key settings for VT input:
	mode |= ENABLE_VIRTUAL_TERMINAL_INPUT;  // Enable VT sequences
	mode &= ~ENABLE_PROCESSED_INPUT;        // Get "raw" escape codes
	mode &= ~ENABLE_LINE_INPUT;             // Non-canonical mode
	mode &= ~ENABLE_ECHO_INPUT;             // No echo

	// Optional for mouse
	// mode |= ENABLE_MOUSE_INPUT;

	if (!SetConsoleMode(term->input, mode)) {
		// Try without ENABLE_VIRTUAL_TERMINAL_INPUT (for older Windows)
		//mode &= ~ENABLE_VIRTUAL_TERMINAL_INPUT;
		//if (!SetConsoleMode(term->input, mode)) {
		//	fprintf(stderr, "Error: SetConsoleMode failed\n");
			return NULL;
		//}
	}

	if (!GetConsoleMode(term->output, &term->original_output_mode)) {
		fprintf(stderr, "Error: GetConsoleMode failed for stdout\n");
		return NULL;
	}

	mode = term->original_output_mode;
	mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
	if (!SetConsoleMode(term->output, mode))
		return NULL;
#else
	// Save current settings
	if (tcgetattr(STDIN_FILENO, &term->original_termios) == -1) {
		fprintf(stderr, "tcgetattr failed\n");
		return NULL;
	}
	
	// Set non-canonical mode
	struct termios raw = term->original_termios;
	raw.c_lflag &= ~(ICANON | ECHO);
	// Additional settings for more "raw" mode:
	raw.c_lflag &= ~ISIG;  // Disable signal handling (Ctrl+C, Ctrl+Z)
	raw.c_iflag &= ~(IXON | ICRNL); // Disable flow control and CR->NL conversion
	
	// Configure minimum number of characters and timeout
	raw.c_cc[VMIN] = 1;   // Read at least 1 character
	raw.c_cc[VTIME] = 0;  // No timeout
	
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) {
		fprintf(stderr, "tcsetattr failed\n");
		return NULL;
	}
#endif
	return term;
}

void restore_terminal(TERM* term) {
	if (term == NULL)
		return;

#ifdef _WIN32
	if (term->input == INVALID_HANDLE_VALUE)
		return;
	
	SetConsoleMode(term->input, term->original_input_mode);

	if (term->output == INVALID_HANDLE_VALUE)
		return;

	SetConsoleMode(term->output, term->original_output_mode);
#else
   
	tcsetattr(STDIN_FILENO, TCSAFLUSH, &term->original_termios);
#endif
	free(term);
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

int terminal_read_char(TERM* term) {
	if (term == NULL)
		return EOF;

	char ch;
#ifdef _WIN32
	while (1) {
		INPUT_RECORD input_record;
		DWORD events_read = 0;

		if (!PeekConsoleInput(term->input, &input_record, 1, &events_read))
			return EOF;

		if (events_read == 0)
			return EOF;

		if (input_record.EventType == KEY_EVENT &&
			input_record.Event.KeyEvent.bKeyDown)
			break;

		ReadConsoleInput(term->input, &input_record, 1, &events_read);
	}

	DWORD charsRead = 0;
	if (!ReadConsoleA(term->input, &ch, 1, &charsRead, NULL))
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
