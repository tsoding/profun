#ifndef GAME_H_
#define GAME_H_

#define N 3
#define SCREEN_WIDTH 640.0
#define SCREEN_HEIGHT 480.0
#define CELL_WIDTH (SCREEN_WIDTH / N)
#define CELL_HEIGHT (SCREEN_HEIGHT / N)


#define EMPTY 0
#define PLAYER_X 1
#define PLAYER_O 2

#define RUNNING_STATE 0
#define PLAYER_X_WON_STATE 1
#define PLAYER_O_WON_STATE 2
#define TIE_STATE 3
#define QUIT_STATE 4

typedef struct {
    int board[N * N];
    int player;
    int state;
} game_t;

#endif  // GAME_H_
