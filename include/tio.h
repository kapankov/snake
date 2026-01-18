#pragma once

#ifdef _WIN32
	#include <windows.h>
#else
	#include <termios.h>
#endif

struct terminal_state;
typedef struct terminal_state TERM;

TERM* init_terminal(void);
void restore_terminal(TERM* term);
void goto_xy(int x, int y);
void cursor_on(void);
void cursor_off(void);
void clear_screen(void);
int terminal_read_char(TERM* term);
