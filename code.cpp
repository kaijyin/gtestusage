/**
 * @file dijkstra.cpp
 * @author kaijintin@gmail.com
 * @brief This file contains the
 * implementation of Dijkstra's
 * algorithm.
 *
 * This implementation is based on the
 * code provided in the lecture slides.
 *
 * @version 0.1
 * @date 2022-03-10
 *
 * @copyright Copyright (c) 2022
 *
 */

#include <algorithm>
#include <climits>
#include <iostream>
#include <queue>
#include <utility>

/**
 * @brief A constant that represents the
 * maximum number of rows and columns in
 * the grid.
 *
 */
const int KMaxN = 1005;

/**
 * @brief A two-dimensional array that
 * stores the distances between each
 * node in the grid.
 *
 */
int Dis[KMaxN][KMaxN];

/**
 * @brief A two-dimensional array that
 * stores the previous node in the
 * shortest path from the source node to
 * each node.
 *
 */
std::pair<int, int> Path[KMaxN][KMaxN];

/**
 * @brief An array of direction vectors
 * that represent the possible
 * neighboring nodes of a given node.
 *
 */
int Dir[8][2] = {{0, -1}, {0, 1},   {1, 0},  {-1, 0},
                 {1, 1},  {-1, -1}, {1, -1}, {-1, 1}};

/**
 * @brief A two-dimensional array that
 * stores whether a given node has been
 * visited or not.
 *
 */
bool Vis[KMaxN][KMaxN];

/**
 * @brief A structure that represents a
 * node in the grid, including its
 * distance from the source node and its
 * position in the grid.
 *
 */
struct Node {
    int Distance;
    int PosX, PosY;

    /**
     * @brief A comparison operator that
     * defines the priority of a node in a
     * priority queue.
     *
     * Nodes with higher distances have
     * higher priority.
     *
     */
    bool operator<(const Node &Other) const {
        return Distance > Other.Distance;
    }
} __attribute__((aligned(16)));

/**
 * @brief A two-dimensional array that
 * stores the edge weights between each
 * pair of nodes.
 *
 */
int Graph[KMaxN][KMaxN];

/**
 * @brief The main function of
 * Dijkstra's algorithm.
 *
 * This function takes the total number
 * of rows and columns in the grid as
 * input, and computes the shortest path
 * from the source node to each node in
 * the grid.
 *
 * @param total_rows The total number of
 * rows in the grid.
 * @param total_cols The total number of
 * columns in the grid.
 *
 */
void dijkstra(int M, int TotalCols) {
    for (int I = 0; I < M; I++) {
        for (int J = 0; J < TotalCols; J++) {
            Dis[I][J] = INT_MIN / 2;  // Initialize, take
                                      // minimum value
            Vis[I][J] = false;
        }
    }

    std::priority_queue<Node> Queue;
    Queue.push({0, 0, 0});
    Dis[0][0] = 0;

    while (!Queue.empty()) {
        int RowPosition = Queue.top().PosX;  // Select an
                                             // optimal node
        int ColumnPosition = Queue.top().PosY;
        Queue.pop();

        if (Vis[RowPosition][ColumnPosition]) {
            continue;
        }
        int now                          = 1;
        Vis[RowPosition][ColumnPosition] = true;

        if (RowPosition == M - 1 &&
            ColumnPosition == TotalCols - 1) {  // Reach the
                                                // destination
                                                // node,
                                                // return
                                                // directly
            return;
        }

        for (auto &I : Dir) {
            int TmpRowPosition    = RowPosition + I[0];
            int TmpColumnPosition = ColumnPosition + I[1];

            if (TmpRowPosition < 0 || TmpRowPosition >= M ||
                TmpColumnPosition < 0 || TmpColumnPosition >= TotalCols ||
                Vis[TmpRowPosition][TmpColumnPosition]) {
                continue;
            }

            if (Dis[RowPosition][ColumnPosition] +
                    Graph[TmpRowPosition][TmpColumnPosition] >
                Dis[TmpRowPosition][TmpColumnPosition]) {
                Dis[TmpRowPosition][TmpColumnPosition] =
                    Dis[RowPosition][ColumnPosition] +
                    Graph[RowPosition][ColumnPosition];
                Path[TmpRowPosition][TmpColumnPosition].first  = RowPosition;
                Path[TmpRowPosition][TmpColumnPosition].second = ColumnPosition;
                Queue.push({Dis[TmpRowPosition][TmpColumnPosition],
                            TmpRowPosition, TmpColumnPosition});
            }
        }
    }
}

/**
 * @brief A recursive function that
 * prints the shortest path from the
 * source node to a given node.
 *
 * This function takes the row and
 * column indices of the destination
 * node as input, and prints the path
 * from the source node to the
 * destination node.
 *
 * @param row_number The row index of
 * the destination node.
 * @param col_number The column index of
 * the destination node.
 *
 */
void printPath(int RowNumber, int ColNumber) {
    if (RowNumber == 0 && ColNumber == 0) {
        std::cout << RowNumber << " " << ColNumber << std::endl;
        return;
    }

    printPath(Path[RowNumber][ColNumber].first,
              Path[RowNumber][ColNumber].second);
    std::cout << RowNumber << " " << ColNumber << std::endl;
}