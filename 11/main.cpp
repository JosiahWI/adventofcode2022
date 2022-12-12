#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <iostream>
#include <vector>

enum class OperationType
{
    Add,
    Mul,
    Square
};

struct Operation
{
    OperationType op{OperationType::Add};
    int val{0};
};

struct Throw
{
    std::uint64_t worry_level{-1U};
    int target{-2};
};

class Monkey
{
public:
    Monkey(const std::vector<int> &starting_items,
            Operation operation,
            int test,
            std::int16_t monkey_true,
            std::int16_t monkey_false)
        : m_items{}
        , m_operation{operation}
        , m_test{test}
        , m_monkey_true{monkey_true}
        , m_monkey_false{monkey_false}
    {
        assert(m_monkey_true >= 0 || m_monkey_false >= 0);
        m_items.resize(starting_items.size());
        std::copy(starting_items.begin(), starting_items.end(), m_items.begin());
    }

    Throw inspect_and_pop(int divisor)
    {
        auto worry_level = m_items.back();
        auto worry_level_pre = worry_level;
        m_items.pop_back();
        switch (m_operation.op) {
        case OperationType::Add:
            worry_level += m_operation.val;
            break;
        case OperationType::Mul:
            worry_level *= m_operation.val;
            break;
        case OperationType::Square:
            worry_level *= worry_level;
            break;
        }
        assert(worry_level >= worry_level_pre);

        #ifdef PART1
            worry_level /= 3;
        #else
            worry_level %= divisor;
        #endif

        if ((worry_level  % m_test) != 0) {
            return {worry_level, m_monkey_false};
        }
        return {worry_level, m_monkey_true};
    }

    void push(int worry_level)
    {
        m_items.push_back(worry_level);
    }

    bool empty() const
    {
        return m_items.empty();
    }

private:
    std::vector<std::uint64_t> m_items;
    Operation m_operation;
    int m_test;
    std::int16_t m_monkey_true;
    std::int16_t m_monkey_false;
};

int main()
{
    std::array<Monkey, 8> monkeys = {
        Monkey({94, 71, 66}, {OperationType::Mul, 5}, 3, 7, 4),
        Monkey({70}, {OperationType::Add, 6}, 17, 3, 0),
        Monkey({78, 94, 65, 56, 68, 62}, {OperationType::Add, 5}, 2, 3, 1),
        Monkey({67, 94, 94, 89}, {OperationType::Add, 2}, 19, 7, 0),
        Monkey({63, 98, 98, 65, 73, 61, 71}, {OperationType::Mul, 7}, 11, 5, 6),
        Monkey({60, 61, 68, 62, 55}, {OperationType::Add, 7}, 5, 2, 1),
        Monkey({71, 50, 89, 72, 64, 69, 91, 93}, {OperationType::Add, 1}, 13, 5, 2),
        Monkey({50, 76}, {OperationType::Square, -1}, 7, 4, 6)};

	#ifdef PART1
		// would cause zero division error if used on part 1
        constexpr int divisor = 0;
        constexpr int iterations = 20;
    #else
        constexpr int divisor = 3 * 17 * 2 * 19 * 11 * 5 * 13 * 7;
        constexpr int iterations = 10000;
    #endif

    std::array<std::int64_t, 8> counts{};

    const auto length = monkeys.size();
    for (int i = 0; i < iterations; ++i) {
        for (std::size_t j = 0; j < length; ++j) {
            while (!monkeys[j].empty()) {
                counts[j] += 1;
                const auto thrown = monkeys[j].inspect_and_pop(divisor);
                monkeys[thrown.target].push(thrown.worry_level);
            }
        }
    }

    for (std::size_t i = 0; i < length; ++i) {
        std::cout << "monkey " << i << " inspected items " << counts[i] << " times\n";
    }
    std::sort(counts.begin(), counts.end(), [](int a, int b) {return a > b;});
    std::cout << counts[0] * counts[1] << '\n';

    return 0;
}
