Unit TIO;

{$mode objfpc} // Modern Object Pascal

Interface

Uses
{$IFDEF LINUX}
	BaseUnix, TermIO,
{$ENDIF}
{$IFDEF WINDOWS}
	Windows,
{$ENDIF}
	SysUtils;

Type
	PTerminalState = ^TTerminalState;
	TTerminalState = Record
		{$IFDEF WIN32}
		Input: THandle;
		Output: THandle;
		OriginalInputMode: DWORD;
		OriginalOutputMode: DWORD;
		{$ELSE}
		OriginalTermios: termios;
		{$ENDIF}
		IsRaw: Boolean;
	End;

Procedure TerminalStateInit(State: PTerminalState);
Procedure InitTerminal(State: PTerminalState);
Procedure RestoreTerminal(State: PTerminalState);
Procedure GotoXY(X, Y: Integer);
Procedure CursorOn;
Procedure CursorOff;
Procedure ClearScreen;
Function TerminalReadChar(State: PTerminalState): Integer;
Function GetChar(State: PTerminalState): Integer;

Implementation

// -------------------------
// Terminal State Management
// -------------------------

Procedure TerminalStateInit(State: PTerminalState);
Begin
	{$IFDEF WIN32}
	State^.Input := INVALID_HANDLE_VALUE;
	State^.OriginalInputMode := 0;
	State^.Output := INVALID_HANDLE_VALUE;
	State^.OriginalOutputMode := 0;
	{$ENDIF}
	State^.IsRaw := False;
End;

Procedure InitTerminal(State: PTerminalState);
Var
{$IFDEF WIN32}
	Mode: DWORD;
{$ELSE}
	Raw: termios;
{$ENDIF}
Begin
	If (State = nil) Or State^.IsRaw Then
		Exit;

	{$IFDEF WIN32}
	State^.Input := GetStdHandle(STD_INPUT_HANDLE);
	If State^.Input = INVALID_HANDLE_VALUE Then
	Begin
		WriteLn(StdErr, 'Error: GetStdHandle(STD_INPUT_HANDLE) failed');
		Exit;
	End;

	State^.Output := GetStdHandle(STD_OUTPUT_HANDLE);
	If State^.Output = INVALID_HANDLE_VALUE Then
	Begin
		WriteLn(StdErr, 'Error: GetStdHandle(STD_OUTPUT_HANDLE) failed');
		Exit;
	End;

	If Not GetConsoleMode(State^.Input, @State^.OriginalInputMode) Then
	Begin
		WriteLn(StdErr, 'Error: GetConsoleMode failed for stdin');
		Exit;
	End;

	Mode := State^.OriginalInputMode;
	Mode := Mode Or ENABLE_VIRTUAL_TERMINAL_INPUT;
	Mode := Mode And Not ENABLE_PROCESSED_INPUT;
	Mode := Mode And Not ENABLE_LINE_INPUT;
	Mode := Mode And Not ENABLE_ECHO_INPUT;

	If Not SetConsoleMode(State^.Input, Mode) Then
		Exit;

	If Not GetConsoleMode(State^.Output, @State^.OriginalOutputMode) Then
	Begin
		WriteLn(StdErr, 'Error: GetConsoleMode failed for stdout');
		Exit;
	End;

	Mode := State^.OriginalOutputMode;
	Mode := Mode Or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
	If Not SetConsoleMode(State^.Output, Mode) Then
		Exit;
	{$ELSE}
	If TCGetAttr(0, State^.OriginalTermios) < 0 Then
	Begin
		WriteLn(StdErr, 'Error: TCGetAttr failed');
		Exit;
	End;
	Raw := State^.OriginalTermios;
	Raw.c_lflag := Raw.c_lflag And Not (ICANON Or ECHO Or ISIG);
	Raw.c_iflag := Raw.c_iflag And Not (IXON Or ICRNL);
	Raw.c_cc[VMIN] := 1;
	Raw.c_cc[VTIME] := 0;

	If TCSetAttr(0, TCSANOW, raw) < 0 Then
	Begin
		WriteLn(StdErr, 'Error: TCSetAttr failed');
		Exit;
	End;
	{$ENDIF}

	State^.IsRaw := True;
End;

Procedure RestoreTerminal(State: PTerminalState);
Begin
	If (State = nil) Or Not State^.IsRaw Then
		Exit;

	{$IFDEF WIN32}
	If State^.Input <> INVALID_HANDLE_VALUE Then
		SetConsoleMode(State^.Input, State^.OriginalInputMode);
	If State^.Output <> INVALID_HANDLE_VALUE Then
		SetConsoleMode(State^.Output, State^.OriginalOutputMode);
	{$ELSE}
	TCSetAttr(0, TCSANOW, State^.OriginalTermios);
	{$ENDIF}

	State^.IsRaw := False;
End;

// -------------------------
// Terminal Control Functions
// -------------------------

Procedure GotoXY(X, Y: Integer);
Begin
	Write(Format(#27'[%d;%dH', [Y, X]));
	Flush(Output);
End;

Procedure CursorOn;
Begin
	Write(#27'[?25h');
	Flush(Output);
End;

Procedure CursorOff;
Begin
	Write(#27'[?25l');
	Flush(Output);
End;

Procedure ClearScreen;
Begin
	Write(#27'[2J'#27'[H');
	Flush(Output);
End;

// -------------------------
// Input Handling
// -------------------------

Function TerminalReadChar(State: PTerminalState): Integer;
Var
		Ch: Char;
	{$IFDEF WIN32}
		InputRecord: TInputRecord;
		EventsRead: DWORD;
		CharsRead: DWORD;
	{$ELSE}
		ReadFds: TFDSet;
		TimeOut: TTimeVal;
	{$ENDIF}
Begin
	Result := -1;
	If (State = nil) Or Not State^.IsRaw Then
		Exit;

	{$IFDEF WIN32}
	While True Do
	Begin
		If Not PeekConsoleInput(State^.Input, InputRecord, 1, EventsRead) Then
			Exit;

		If EventsRead = 0 Then
			Exit;

		If (InputRecord.EventType = KEY_EVENT) And 
			(InputRecord.Event.KeyEvent.bKeyDown) Then
			Break;

		ReadConsoleInput(State^.Input, InputRecord, 1, EventsRead);
	End;

	If Not ReadConsoleA(State^.Input, @Ch, 1, CharsRead, nil) Then
		Exit;
	Result := Ord(Ch);
	{$ELSE}
	TimeOut.tv_sec := 0;
	TimeOut.tv_usec := 0;

	fpFD_ZERO(ReadFds);
	fpFD_SET(StdInputHandle, ReadFds);
	
	If (fpSelect(StdInputHandle + 1, @ReadFds, nil, nil, @TimeOut) > 0) And 
		(fpFD_ISSET(StdInputHandle, ReadFds) > 0) Then
	Begin
		If fpRead(StdInputHandle, @Ch, 1) <> 1 Then
			Exit;
	End
	Else
		Exit;
	{$ENDIF}
End;

Function GetChar(State: PTerminalState): Integer;
type
	TParserState = (
		STATE_NORMAL,
		STATE_ESC,
		STATE_CSI,
		STATE_OSC,
		STATE_SS3,
		STATE_IGNORE
	);
Var
	ParserState: TParserState;
	OscEsc: Boolean;
	Ch: Integer;
Begin
	ParserState := STATE_NORMAL;
	OscEsc := False;

	while True Do
	Begin
		Ch := TerminalReadChar(State);
		If Ch = -1 Then
		Begin
			Result := -1;
			Exit;
		End;

		Case ParserState Of
			STATE_NORMAL:
				If Ch = 27 Then
					ParserState := STATE_ESC
				Else
					Break;

			STATE_ESC:
				If Ch = Ord('[') Then
					ParserState := STATE_CSI
				Else If Ch = Ord(']') Then
				Begin
					ParserState := STATE_OSC;
					OscEsc := False;
				end
				Else If Ch = Ord('O') Then
					ParserState := STATE_SS3
				Else
					Break;

			STATE_CSI:
				If (Ch >= $40) And (Ch <= $7E) Then
					ParserState := STATE_NORMAL;

			STATE_OSC:
				Begin
					If OscEsc Then
					Begin
						If Ch = Ord('\') Then
							ParserState := STATE_NORMAL;
						OscEsc := False;
					end
					Else If Ch = 27 Then
						OscEsc := True
					Else If Ch = Ord(#7) Then // BEL
						ParserState := STATE_NORMAL;
				End;

			STATE_SS3:
				ParserState := STATE_NORMAL;

			STATE_IGNORE:
				If (Ch >= 32) And (Ch <= 126) Then
					ParserState := STATE_NORMAL;
		End;
	End;

	Result := Ch;
End;

End.
