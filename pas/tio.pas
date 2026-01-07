Unit TIO;

Interface

Uses
{$IFDEF LINUX}
	BaseUnix, TermIO,
{$ENDIF}
{$IFDEF WINDOWS}
	Windows,
{$ENDIF}
	SysUtils;

{$IFDEF LINUX}
Var
	OriginalTermios: TermIOS;
{$ENDIF}

/// Clears the screen and moves cursor to top-left corner
Procedure ClearScreen;
/// Moves cursor to specified coordinates
/// @param X horizontal position
/// @param Y vertical position
Procedure GotoXY(X, Y: Integer);
/// Turns on cursor visibility
Procedure CursorOn;
/// Turns off cursor visibility
Procedure CursorOff;
/// Initializes terminal for non-canonical input mode
Procedure InitTerminal;
/// Restores original terminal settings
Procedure RestoreTerminal;
/// Non-blocking retrieval of the pressed key code
Function GetKey: Integer;

Implementation

Procedure ClearScreen;
Begin
	Write(#27'[2J');
	Write(#27'[H');
End;

Procedure GotoXY(X, Y: Integer);
Begin
	Write(#27'[', Y, ';', X, 'H');
	Flush(Output);
End;

Procedure CursorOn;
Begin
	Write(#27'[?25h');
	Flush(Output)
End;

Procedure CursorOff;
Begin
	Write(#27'[?25l');
	Flush(Output)
End;

Procedure InitTerminal;
{$IFDEF LINUX}
Var
	NewTerm: TermIOS;
Begin
	// Save the original settings
	TCGetAttr(0, OriginalTermios);

	// Set non-canonical mode
	NewTerm := OriginalTermios;
	NewTerm.c_lflag := NewTerm.c_lflag and (not (ICANON or ECHO));
	NewTerm.c_cc[VMIN] := 0;   // Non-blocking input
	NewTerm.c_cc[VTIME] := 0;  // No timeout
	TCSetAttr(0, TCSANOW, NewTerm);
{$ENDIF}
{$IFDEF WINDOWS}
Var
	ConsoleHandle: THandle;
	ConsoleMode: DWORD;
Begin
	// For Windows enable ANSI escape code support
	// Get console handle
	ConsoleHandle := GetStdHandle(STD_OUTPUT_HANDLE);
	If GetConsoleMode(ConsoleHandle, @ConsoleMode) Then
	Begin
		// Enable ENABLE_VIRTUAL_TERMINAL_PROCESSING flag
		SetConsoleMode(ConsoleHandle, ConsoleMode OR ENABLE_VIRTUAL_TERMINAL_PROCESSING);
	End;
{$ENDIF}
	CursorOff
End;

Procedure RestoreTerminal;
Begin
	{$IFDEF LINUX}
	// Restore the original settings
	TCSetAttr(0, TCSANOW, OriginalTermios);
	{$ENDIF}

	CursorOn
End;

Function GetKey: Integer;
{$IFDEF WINDOWS}
Var
	ConsoleHandle: THandle;
	NumPending: DWord;
	InputRecord: TInputRecord;
	NumRead: DWord;
	KeyDown: Boolean;
Begin
	ConsoleHandle := GetStdHandle(STD_INPUT_HANDLE);

	If not GetNumberOfConsoleInputEvents(ConsoleHandle, NumPending) Then
	Begin
		GetKey := -1;
		Exit;
	End;

	If NumPending = 0 Then
	Begin
		GetKey := -1;
		Exit;
	End;

	While NumPending > 0 Do
	Begin
		If ReadConsoleInput(ConsoleHandle, InputRecord, 1, NumRead) Then
		Begin
			If InputRecord.EventType = KEY_EVENT Then
			Begin
				KeyDown := InputRecord.Event.KeyEvent.bKeyDown;
				If KeyDown Then
				Begin
					If InputRecord.Event.KeyEvent.AsciiChar <> #0 Then
					Begin
						GetKey := Integer(InputRecord.Event.KeyEvent.AsciiChar);
						Exit;
					End
					Else
					Begin
						GetKey := InputRecord.Event.KeyEvent.wVirtualKeyCode;
						Exit;
					End;
				End;
			End;
		End;
		Dec(NumPending);
	End;
	GetKey := -1;
End;
{$ENDIF}
{$IFDEF LINUX}
Var
	BytesAvailable: Integer;
	Ch: Char;
	NextCh: Char;
	ThirdCh: Char;
Begin
	// Check if there are data available to read
	FpIOCtl(0, FIONREAD, @BytesAvailable);
	
	// If no bytes available, return -1
	If BytesAvailable = 0 Then
	Begin
		GetKey := -1;
		Exit;
	End;
	
	// Read the first byte
	Read(Ch);
	
	// If this is ESC (0x1B), it's the beginning of an escape sequence
	If Ord(Ch) = 27 Then
	Begin
		// Check if there are more bytes to read
		If BytesAvailable > 0 Then
		Begin
			// Read the next byte
			Read(NextCh);
			// If this is '[' - it's the beginning of arrow keys escape sequence
			If NextCh = '[' Then
			Begin
				// Skip the remaining bytes of the arrow keys sequence
				// Read and check the type of sequence
				// Just skip all characters until the end of sequence (until we meet a letter)
				While BytesAvailable > 0 Do
				Begin
					Read(Ch);
					// If the character is a letter (ASCII 65-90 or 97-122), then this is the end of sequence
					If (Ch >= 'A') And (Ch <= 'Z') Or (Ch >= 'a') And (Ch <= 'z') Then
						Break;
					Dec(BytesAvailable);
				End;
				// Return -1 to ignore the whole escape sequence
				GetKey := -1;
			End
			Else If Ord(NextCh) = 79 Then // 79 = 'O' - for function keys F1-F12
			Begin
				// Read the third byte
				If BytesAvailable > 0 Then
				Begin
					Read(ThirdCh);
					// Skip the function key (F1-F12)
					// Return -1 to ignore the whole escape sequence
					GetKey := -1;
				End
				Else
				Begin
					// If there is no third byte, return the 'O' character
					GetKey := Ord(NextCh);
				End;
			End
			Else
			Begin
				// This is not an arrow keys or function keys escape sequence, return the character
				GetKey := Ord(Ch); // Return the first character
			End;
		End
		Else
		Begin
			// No additional bytes, return ESC
			GetKey := Ord(Ch);
		End;
	End
	Else
	Begin
		// Return the regular character
		GetKey := Ord(Ch);
	End;
End;
{$ENDIF}

End.
