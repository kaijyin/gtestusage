#include <gtest/gtest.h>
#include <limits.h>

#include <sstream>
#include <vector>

// Include the file where the Dijkstra and printPath functions are defined.
// Assuming the file is named dijkstra.h for this example.
#include "code.cpp"

// Helper function to set up a graph for testing
void SetUpGraph(int Graph[KMaxN][KMaxN],
                const std::vector<std::vector<int>> &input) {
    for (int i = 0; i < input.size(); ++i) {
        for (unsigned long j = 0; j < input[i].size(); ++j) {
            Graph[i][j] = input[i][j];
        }
    }
}

// Test fixture for Dijkstra tests
class DijkstraTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Reset the graph to -1 indicating no path
        for (int i = 0; i < KMaxN; ++i) {
            for (int j = 0; j < KMaxN; ++j) {
                Graph[i][j] = -1;
            }
        }
    }
};

TEST_F(DijkstraTest, FindsShortestPathInSimpleGraph) {
    // Set up a simple graph
    // Graph:
    // 0 1 2
    // 3 4 5
    // 6 7 8
    std::vector<std::vector<int>> input = {
        {0, 1, 100}, {1, 100, 1}, {100, 1, 0}};
    SetUpGraph(Graph, input);

    // Expected shortest path distance from (0,0) to (2,2) is 3
    int expected_distance = 3;
    // Expected path is (0,0) -> (0,1) -> (1,2) -> (2,2)
    std::string expected_path = "0 0\n0 1\n1 2\n2 2\n";

    dijkstra(3, 3);  // Run Dijkstra on the graph

    // Check if the distance to (2,2) is as expected
    ASSERT_EQ(Dis[2][2], expected_distance);

    // Redirect cout to capture the output of printPath
    std::streambuf *old_cout_stream_buf = std::cout.rdbuf();
    std::ostringstream str_cout;
    std::cout.rdbuf(str_cout.rdbuf());

    // Print the path and restore old cout buffer
    printPath(2, 2);
    std::cout.rdbuf(old_cout_stream_buf);

    // Check if the printed path is as expected
    ASSERT_EQ(str_cout.str(), expected_path);
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}