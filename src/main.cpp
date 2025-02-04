#include <chrono>
#include <thread>
#include <iostream>

namespace game_engine
{
	template<typename T>
	class game
	{
	protected:
		void run()
		{
			running_ = true;
			while (running_)
			{
				auto derived = static_cast<T*>(this);
				derived->draw();
				derived->input();
				derived->logic();
				sleep();
			}
		};
		void stop()
		{
			running_ = false;
		}
		int64_t get_sleep_time()
		{
			return sleep_time_;
		}
		void set_sleep_time(int64_t value)
		{
			sleep_time_ = value;
		}
	private:
		void sleep() const
		{
			std::this_thread::sleep_for(std::chrono::milliseconds(sleep_time_));
		};
	private:
		int64_t sleep_time_{ 100 }; // milliseconds
		bool running_{ false };
	};
}

class snake : public game_engine::game<snake>
{
	friend class game_engine::game<snake>;
public:
	int run()
	{
		game_engine::game<snake>::run();
		return score_;
	}
private:
	void draw()
	{

	}
	void input()
	{

	}
	void logic()
	{
		++score_;
		if (score_ >= 50)
			stop();
	}
private:
	int score_{ 0 };
};

int main()
{
	snake instance;
	auto score = instance.run();
	std::cout << "Score: " << score << std::endl;
	return 0;
}

