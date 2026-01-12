Program Snake;

{$mode objfpc} // Modern Object Pascal

Uses
	tio,
	SysUtils;

Const
	GamePause = 100;
	GameFrameWidth = 40;
	GameFrameHeight = 24;

Type
	PPoint = ^TPoint;
	TPoint = Record
		X, Y: Integer;
	End;

	PNode = ^TNode;
	TNode = Record
		Data: TPoint;
		Next: PNode;
	End;

	PTList = ^TList;
	TList = Record
		Head: PNode;
	End;

	TGameAction = (
		Waiting,
		GameOver,
		MoveUp,
		MoveDown,
		MoveLeft,
		MoveRight
	);

	PGameState = ^TGameState;
	TGameState = Record
		Action: TGameAction;
		FrameWidth, FrameHeight: Integer;
		Fruit: TPoint;
		SnakeBody: TList;
		Score: Integer;
	End;

// -------------------------
// Linked List (Snake Body)
// -------------------------

Procedure PushHead(List: PTList; Pt: TPoint);
Var
	NewNode: PNode;
Begin
	New(NewNode);
	NewNode^.Data := Pt;
	NewNode^.Next := List^.Head;
	List^.Head := NewNode;
End;

Procedure PopTail(List: PTList);
Var
	Tail, Prev: PNode;
Begin
	Tail := List^.Head;
	Prev := nil;

	while (Tail <> nil) And (Tail^.Next <> nil) Do
	Begin
		Prev := Tail;
		Tail := Tail^.Next;
	End;

	If Prev <> nil Then
		Prev^.Next := nil
	Else
		List^.Head := nil;

	Dispose(Tail);
End;

// -------------------------
// Game
// -------------------------

Function PointEqual(Pt1, Pt2: PPoint): Boolean;
Begin
	Result := (Pt1^.X = Pt2^.X) And (Pt1^.Y = Pt2^.Y);
End;

Function GetRandomPoint: TPoint;
Begin
	Result.X := Random(GameFrameWidth - 6) + 3;
	Result.Y := Random(GameFrameHeight - 6) + 3;
End;

Procedure DrawFrame(Width, Height: Integer);
Var
	Column, Row: Integer;
Begin
	GotoXY(1, 1);
	For Column := 0 to Width - 1 Do
		Write('#');

	For Row := 2 to Height - 1 Do
	Begin
		GotoXY(1, Row);
		Write('#');
		For Column := 1 to Width - 2 Do
			Write(' ');
		Write('#');
	End;

	GotoXY(1, Height);
	For Column := 0 to Width - 1 Do
		Write('#');
	Flush(Output);
End;

Procedure Draw(State: PGameState);
Var
	TailNode: PNode;
	Pt: TPoint;
Begin
	TailNode := State^.SnakeBody.Head;
	Pt := TailNode^.Data;
	GotoXY(Pt.X, Pt.Y);
	Write('O');
	Flush(Output);

	while TailNode^.Next <> nil Do
		TailNode := TailNode^.Next;

	If TailNode <> State^.SnakeBody.Head Then
	Begin
		Pt := TailNode^.Data;
		GotoXY(Pt.X, Pt.Y);
		Write(' ');
	End;

	GotoXY(State^.Fruit.X, State^.Fruit.Y);
	Write('F');
	Flush(Output);
End;

Procedure GetInput(Ch: Integer; State: PGameState);
Begin
	If Ch = -1 Then
		Exit;

	case Chr(Ch) of
		'w', 'W': If State^.Action <> MoveDown Then State^.Action := MoveUp;
		's', 'S': If State^.Action <> MoveUp Then State^.Action := MoveDown;
		'a', 'A': If State^.Action <> MoveRight Then State^.Action := MoveLeft;
		'd', 'D': If State^.Action <> MoveLeft Then State^.Action := MoveRight;
		'q', 'Q': State^.Action := GameOver;
	End;
End;

Procedure UpdateFruit(State: PGameState);
Var
	Found: Boolean;
	TailNode: PNode;
Begin
	Found := False;
	while Not Found Do
	Begin
		State^.Fruit := GetRandomPoint;
		Found := True;
		TailNode := State^.SnakeBody.Head;
		while TailNode <> nil Do
		Begin
			If PointEqual(@State^.Fruit, @TailNode^.Data) Then
			Begin
				Found := False;
				Break;
			End;
			TailNode := TailNode^.Next;
		End;
	End;
End;

Procedure DoLogic(State: PGameState);
Var
	Head: TPoint;
	Goal: Boolean;
Begin
	If State^.Action = Waiting Then
		Exit;

	Head := State^.SnakeBody.Head^.Data;

	case State^.Action of
		MoveUp: Dec(Head.Y);
		MoveDown: Inc(Head.Y);
		MoveLeft: Dec(Head.X);
		MoveRight: Inc(Head.X);
	End;

	If (Head.X < 2) Or (Head.X > State^.FrameWidth - 1) Or
		 (Head.Y < 2) Or (Head.Y > State^.FrameHeight - 1) Then
	Begin
		State^.Action := GameOver;
		Exit;
	End;

	Goal := PointEqual(@State^.Fruit, @Head);

	If Not Goal And (State^.SnakeBody.Head^.Next <> nil) Then
		PopTail(@State^.SnakeBody);

	PushHead(@State^.SnakeBody, Head);

	If Goal Then
	Begin
		Inc(State^.Score, 10);
		UpdateFruit(State);
	End;
End;

// -------------------------
// Main
// -------------------------

Var
	TermState: TTerminalState;
	GameState: TGameState;

Begin
	Randomize;
	TerminalStateInit(@TermState);
	InitTerminal(@TermState);

	GameState.Action := Waiting;
	GameState.FrameWidth := GameFrameWidth;
	GameState.FrameHeight := GameFrameHeight;
	GameState.Fruit.X := GameFrameWidth Div 2;
	GameState.Fruit.Y := GameFrameHeight Div 2;
	GameState.SnakeBody.Head := nil;
	GameState.Score := 0;

	PushHead(@GameState.SnakeBody, GetRandomPoint);

	CursorOff;
	ClearScreen;
	DrawFrame(GameState.FrameWidth, GameState.FrameHeight);

	While True Do
	Begin
		Draw(@GameState);
		GetInput(GetChar(@TermState), @GameState);
		DoLogic(@GameState);
		If GameState.Action = GameOver Then
			Break;
		Sleep(GamePause);
	End;

	ClearScreen;
	CursorOn;
	WriteLn('Score: ', GameState.Score);
	RestoreTerminal(@TermState);
End.
