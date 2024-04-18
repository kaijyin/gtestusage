#include <algorithm>
#include <iostream>
#include <limits.h>
#include <queue>
#include <utility>

const int MAX_N = 1005;
int dis[MAX_N][MAX_N];
std::pair<int, int> path[MAX_N][MAX_N];
int dir[8][2] = {{0, -1}, {0, 1},   {1, 0},  {-1, 0},
                 {1, 1},  {-1, -1}, {1, -1}, {-1, 1}};
bool vis[MAX_N][MAX_N];
struct Node {
  int distance;
  int pos_x, pos_y;
  bool operator<(const Node &other) const {
    //优先队列默认为最大堆，无需反转条件。
    return distance > other.distance;
  }
};
int graph[MAX_N][MAX_N];
void Djkstra(int totoal_rows, int total_cols) {
  for (int i = 0; i < totoal_rows; i++) {
    for (int j = 0; j < total_cols; j++) {
      dis[i][j] = INT_MIN / 2; //初始化，取最小值
      vis[i][j] = false;
    }
  }
  std::priority_queue<Node> queue;
  queue.push({0, 0, 0});
  dis[0][0] = 0;
  while (!queue.empty()) {
    int row_position = queue.top().pos_x; //选出一个当前最优解
    int column_position = queue.top().pos_y;
    queue.pop();
    if (vis[row_position][column_position])
      continue;
    vis[row_position][column_position] = true;
    if (row_position == totoal_rows - 1 &&
        column_position == total_cols - 1) { //走到终点，直接返回
      return;
    }
    for (int i = 0; i < 8; i++) {
      int tmp_row_position = row_position + dir[i][0];
      int tmp_column_position = column_position + dir[i][1];
      if (tmp_row_position < 0 || tmp_row_position >= totoal_rows ||
          tmp_column_position < 0 || tmp_column_position >= total_cols ||
          vis[tmp_row_position][tmp_column_position]) {
        continue;
      }
      if (dis[row_position][column_position] +
              graph[tmp_row_position][tmp_column_position] >
          dis[tmp_row_position][tmp_column_position]) { //选择权值最大的路径
        dis[tmp_row_position][tmp_column_position] =
            dis[row_position][column_position] +
            graph[row_position][column_position];
        path[tmp_row_position][tmp_column_position].first =
            row_position; //更新到当前点的path
        path[tmp_row_position][tmp_column_position].second = column_position;
        queue.push({dis[tmp_row_position][tmp_column_position],
                    tmp_row_position, tmp_column_position});
      }
    }
  }
}
void printPath(int row_number, int col_number) { //递归打印path
  if (row_number == 0 && col_number == 0) {
    std::cout << row_number << " " << col_number << std::endl;
    return;
  }
  printPath(path[row_number][col_number].first,
            path[row_number][col_number].second);
  std::cout << row_number << " " << col_number << std::endl;
}