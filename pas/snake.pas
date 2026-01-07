Program Snake;
Uses
	TIO,
	SysUtils;

Const
	Pause = 100;
	GameFrameWidth = 40;
	GameFrameHeight = 24;

Type
	GameAction = (Waiting, GameOver, MoveUp, MoveDown, MoveLeft, MoveRight);
	TPoint = Record
		X: Integer;
		Y: Integer
	End;
	
	// Linked list fo snake body
	PNode = ^TNode;
	TNode = Record
		Data: TPoint;
		Next: PNode
	End;
	TLinkedList = Record
		Head: PNode;
	End;
	// Game state
	TGameState = Record
		Action: GameAction;
		FrameWidth: Integer;
		FrameHeight: Integer;
		Fruit: TPoint;
		SnakeBody: TLinkedList;
		Score: Integer
	End;


// Function to compare two points
Function PointEqual(const A, B: TPoint): Boolean;
Begin
	PointEqual := (A.X = B.X) And (A.Y = B.Y);
End;

// ######## forward list ########
// insert at begin
Procedure ListPush(Var List: TLinkedList; Point: TPoint);
Var
	NewNode: PNode;
Begin
	New(NewNode);
	NewNode^.Data := Point;
	NewNode^.Next := List.Head;
	List.Head := NewNode
End;

// remove last element
Procedure ListPop(Var List: TLinkedList);
Var
	TailNode: PNode;
	PrevNode: PNode;
Begin
	If List.Head = nil Then
		Exit;
		
	TailNode := List.Head;
	PrevNode := nil;
	
	// Find the last node
	While TailNode^.Next <> nil Do
	Begin
		PrevNode := TailNode;
		TailNode := TailNode^.Next;
	End;
	
	// If it's the only node in the list
	If PrevNode = nil Then
		List.Head := nil
	Else
		PrevNode^.Next := nil;
		
	// Dispose the last node
	Dispose(TailNode);
End;

// ######## game ########

Procedure DrawFrame(Width: Integer; Height: Integer);
Var
	Row, Column: Integer;
Begin
	GotoXY(1, 1);
	For Column := 1 To Width Do
		write('#');
	For Row := 2 To Height - 1 Do
	Begin
		GotoXY(1, Row);
		write('#');
		For Column := 1 To Width - 2 Do
			write(' ');
		write('#')
	End;
	GotoXY(1, Height);
	For Column := 0 To Width - 1 Do
		write('#')
End;

Procedure Draw(State: TGameState);
Var
	TailNode: PNode;
	Point: TPoint;
Begin
	// Draw the snake
	TailNode := State.SnakeBody.Head;
	Point := TailNode^.Data;
	GotoXY(Point.X, Point.Y);
	Write('0');

	While TailNode^.Next<>nil Do
		TailNode := TailNode^.Next;
	
	If TailNode <> State.SnakeBody.Head Then
	Begin
		Point := TailNode^.Data;
		GotoXY(Point.X, Point.Y);
		Write(' ');		
	End;

	// Draw the fruit
	GotoXY(State.Fruit.X, State.Fruit.Y);
	Write('F');

End;

Function GetRandomPoint(): TPoint;
Var
	Point: TPoint;
Begin
	Point.X := Random(GameFrameWidth - 6) + 3;
	Point.Y := Random(GameFrameHeight - 6) + 3;
	GetRandomPoint := Point
End;

Procedure UpdateFruit(Var State: TGameState);
Var
	TailNode: PNode;
	Found: Boolean;
Begin
	Found := False;
	While Found = False Do
	Begin
		State.Fruit := GetRandomPoint();
		Found := True;
		TailNode := State.SnakeBody.Head;
		While TailNode<>nil Do
		Begin
			If PointEqual(State.Fruit, TailNode^.Data) Then
			Begin
				Found := False;
				Break;
			End;
			TailNode := TailNode^.Next;
		End;
	End;
End;

Procedure DoLogic(Var State: TGameState);
Var
	Head: TPoint;
	Goal: Boolean;
Begin
	If State.Action = Waiting Then
		Exit;
	Head := State.SnakeBody.Head^.Data;
	Case State.Action Of
	MoveUp: Dec(Head.Y);
	MoveDown: Inc(Head.Y);
	MoveLeft: Dec(Head.X);
	MoveRight: Inc(Head.X);
	End;
	// Check if head is out of bounds
	If (Head.X < 2) Or (Head.X > State.FrameWidth - 1) Or (Head.Y < 2) Or (Head.Y > State.FrameHeight - 1) Then
	Begin
		State.Action := GameOver;
		Exit;
	End;
	
	// Check the fruit
	Goal := PointEqual(State.Fruit, Head);

	If (Not Goal) And (State.SnakeBody.Head^.Next <> nil) Then
		ListPop(State.SnakeBody);
	ListPush(State.SnakeBody, Head);
	
	If Goal Then
	Begin
		Inc(State.Score, 10);
		UpdateFruit(State)
	End;
End;

Procedure GetInput(var Action: GameAction);
Var
	Key: Integer;
Begin
	Key := GetKey;
	if Key = -1 Then
		Exit;
	Case Chr(Key) Of
	'w', 'W': 
		If Action <> MoveDown Then
			Action := MoveUp;
	's', 'S':
		If Action <> MoveUp Then
			Action := MoveDown;
	'a', 'A':
		If Action <> MoveRight Then
			Action := MoveLeft;
	'd', 'D':
		If Action <> MoveLeft Then
			Action := MoveRight;
	'q', 'Q': Action := GameOver
	End;
End;

// Main
Var
	State: TGameState = (
		Action: Waiting;
		FrameWidth: GameFrameWidth;
		FrameHeight: GameFrameHeight;
		Fruit: (
			X: GameFrameWidth div 2;
			Y: GameFrameHeight div 2
			);
		SnakeBody: (
			Head: nil
			);
		Score: 0
		);
Begin
	InitTerminal;
	ClearScreen;
	Randomize;
	// Add snake head
	ListPush(State.SnakeBody, GetRandomPoint());
	DrawFrame(State.FrameWidth, State.FrameHeight);
	While True Do
	Begin
		Draw(State);
		GetInput(State.Action);
		If State.Action = GameOver Then
			Break;
		DoLogic(State);
		Sleep(Pause)
	End;

	ClearScreen;
	WriteLn('Score: ', State.Score);
	RestoreTerminal
End.
