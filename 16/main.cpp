#include <algorithm>
#include <array>
#include <cassert>
#include <fstream>
#include <iostream>
#include <istream>
#include <map>
#include <queue>
#include <sstream>
#include <string>
#include <vector>

static constexpr int max_flow_rate = 100;
static constexpr int start_id =
    26 * static_cast<int>('A') + static_cast<int>('A');
static constexpr int end_id =
    26 * static_cast<int>('Z') + static_cast<int>('Z');

struct Valve
{
    std::vector<int> connections{};
    int flow{0};
    int id{0};

    bool operator<(const Valve& other) const {
        return this->flow > other.flow;
    }
};

struct State
{
    std::vector<int> visited{};
    int flow{0};
    int acc{0};
    int minutes_left{26};
};

class DistTree
{
public:
    DistTree() : m_distances{}
    {
        for (auto& i : m_distances) {
            i = -1;
        }
    }

    int get_distance(const int from_id, const int to_id) const
    {
        assert(from_id >= start_id && from_id <= end_id);
        assert(to_id >= start_id && to_id <= end_id);
        return m_distances[676 * (from_id - start_id) + (to_id - start_id)];
    }
    
    void set_distance(const int from_id, const int to_id, const int distance)
    {
        assert(from_id >= start_id && from_id <= end_id);
        assert(to_id >= start_id && to_id <= end_id);
        m_distances[676 * (from_id - start_id) + (to_id - start_id)] = distance;
    }

private:
    std::array<int, 456976> m_distances;
};

Valve parse_valve(std::istream& is)
{
    Valve v{};
    is >> v.id >> v.flow;
    assert(v.flow < max_flow_rate);
    while (!is.eof()) {
        is >> v.connections.emplace_back();
    }
    
    if (is.fail()) {
        std::cout << "invalid input\n";
    }
    
    return v;
}

int find_max_pressure_release(const std::map<int, Valve>& valves,
        const DistTree& dist_tree, const Valve& start,
        const State state)
{
    static bool player_2 = false;
    if (state.minutes_left <= 0) {
        if (player_2) {
            return state.acc;
        }

        const auto start_valve = valves.at(start_id);
        State new_state{
            state.visited,
            0,
            0,
            26};
        player_2 = true;
        const auto best_release_second =
            find_max_pressure_release(valves, dist_tree, start_valve, new_state);
        player_2 = false;

        return state.acc + best_release_second;
    }

    int best_pressure_release = -1;
    for (const auto&[to_id, to_valve]: valves) {
        if (assert(to_valve.flow >= 0); to_valve.flow == 0) {
            continue;
        } else if (std::find(
                state.visited.begin(), state.visited.end(), to_id)
                != state.visited.end()) {
            continue;
        }

        const auto minutes_elapsed_walking =
            dist_tree.get_distance(start.id, to_id);
        assert(minutes_elapsed_walking >= 0);
        // opening the valve takes an extra minute
        State new_state{
            state.visited,
            state.flow + to_valve.flow,
            state.acc + (
                std::min(minutes_elapsed_walking + 1, state.minutes_left))
                * state.flow,
            state.minutes_left - minutes_elapsed_walking - 1};
        new_state.visited.push_back(to_id);

        if (auto pressure_release =
                find_max_pressure_release(
                    valves, dist_tree, to_valve, new_state);
                pressure_release > best_pressure_release) {
            best_pressure_release = pressure_release;
        }
    }

    if (best_pressure_release == -1) {
        return state.flow * state.minutes_left + state.acc;
    }

    return best_pressure_release;
}



DistTree build_dist_tree(const std::map<int, Valve>& valves)
{
    DistTree dist_tree{};
    for (const auto&[from_id, from_valve]: valves) {
        if (assert(from_valve.flow >= 0); from_valve.flow == 0 && from_valve.id != start_id) {
            continue;
        }

        dist_tree.set_distance(from_id, from_id, 0);
        std::queue<Valve> bfs_queue{};
        bfs_queue.emplace(from_valve);
        while (!bfs_queue.empty()) {
            const auto& next = bfs_queue.front();
            for (const auto to_id : next.connections) {
                if (dist_tree.get_distance(from_id, to_id) != -1) {
                    continue;
                }
                const auto to_valve = valves.at(to_id);
                const auto cur_dist = dist_tree.get_distance(from_id, next.id);
                dist_tree.set_distance(from_id, to_id, cur_dist + 1);
                bfs_queue.emplace(to_valve);
            }
            bfs_queue.pop();
        }
    }
    return dist_tree;
}

int main()
{
    std::map<int, Valve> valves;
    std::ifstream ifs("modded_input.txt");
    std::string line{};
    while (std::getline(ifs, line)) {
        std::stringstream ss(line);
        const auto valve = parse_valve(ss);
        valves.emplace(valve.id, valve);
    }

    if (const auto& start = valves.find(start_id); start != valves.end()) {
        auto dist_tree = build_dist_tree(valves);
        const auto max_flow =
            find_max_pressure_release(valves, dist_tree, start->second, {});
        std::cout << max_flow << '\n';
    } else {
        std::cout << "start valve not located in cave\n";
    }

    return 0;
}
