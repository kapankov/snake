/**
 * @file snake.cpp
 * @author Konstantin A. Pankov(explorus@mail.ru)
 *
 * @license Licensed under the MIT License, Version 2.0 (see LICENSE.txt).
 *
 * @brief Snake game implementation using Windows Classic Console API
 */

#include <chrono>
#include <thread>
#include <iostream>
#include <random>
#include <list>

#define NOMINMAX
#include <windows.h>

namespace conio
{
	HANDLE open_input()
	{
		return ::CreateFileW(
			L"CONIN$",
			GENERIC_READ | GENERIC_WRITE,
			FILE_SHARE_READ | FILE_SHARE_WRITE,
			nullptr,
			OPEN_EXISTING,
			0,
			nullptr);
	}

	void close_input(HANDLE console_input_handle)
	{
		::CloseHandle(console_input_handle);
	}

	int get_key(HANDLE console_input_handle)
	{
		if (console_input_handle == INVALID_HANDLE_VALUE)
			return -1;

		DWORD num_pending = 0;
		if (!::GetNumberOfConsoleInputEvents(console_input_handle, &num_pending))
			return -1;

		if (num_pending == 0)
			return -1;

		INPUT_RECORD record;
		DWORD num_read = 0;
		while (num_pending--)
		{
			::ReadConsoleInputW(console_input_handle, &record, 1, &num_read);
			if (record.EventType != KEY_EVENT || !record.Event.KeyEvent.bKeyDown)
				continue;

			if (record.Event.KeyEvent.uChar.AsciiChar)
				return record.Event.KeyEvent.uChar.AsciiChar;
			else
			{
				int result = record.Event.KeyEvent.wVirtualKeyCode;
				return result << 16;
			}
		}

		return -1;
	}

	bool set_cursor_visible(HANDLE h_screen_buffer, bool visible)
	{
		CONSOLE_CURSOR_INFO cursor_info;
		::GetConsoleCursorInfo(h_screen_buffer, &cursor_info);
		bool result = cursor_info.bVisible;
		cursor_info.bVisible = visible;
		::SetConsoleCursorInfo(h_screen_buffer, &cursor_info);
		return result;
	}

	COORD get_screen_size(HANDLE h_std_out)
	{
		CONSOLE_SCREEN_BUFFER_INFO csbi;
		::GetConsoleScreenBufferInfo(h_std_out, &csbi);
		return csbi.dwSize;
	}
}

namespace game_engine
{
	template<typename T>
	class game
	{
	protected:
		void run()
		{
			std::unique_ptr<void, void(*)(void*)> input(conio::open_input(), conio::close_input);
			running_ = true;
			while (running_)
			{
				auto derived = static_cast<T*>(this);
				derived->on_draw();
				auto key = conio::get_key(input.get());
				if (key != -1)
					derived->on_input(key);
				derived->on_logic();
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

template<typename T>
class random_int
{
public:
	T get(T from, T to)
	{
		std::uniform_int_distribution<T> dist{ from, to };
		return dist(mt_);
	}
private:
	std::random_device rd_;
	std::mt19937 mt_{ rd_() };
};

class render final
{
public:
	render(short x, short y)
	{
		h_std_out_ = ::GetStdHandle(STD_OUTPUT_HANDLE);
		screen_size_ = COORD{x, y};
		const auto buff_size = screen_size_.X * screen_size_.Y;
		buff_ = std::make_unique<CHAR_INFO[]>(buff_size);
		memset(buff_.get(), 0, buff_size * sizeof(CHAR_INFO));

		h_screen_buffer_ = ::CreateConsoleScreenBuffer(
			GENERIC_READ | GENERIC_WRITE,
			FILE_SHARE_READ | FILE_SHARE_WRITE,
			nullptr,
			CONSOLE_TEXTMODE_BUFFER,
			nullptr);
		::SetConsoleActiveScreenBuffer(h_screen_buffer_);
		conio::set_cursor_visible(h_screen_buffer_, false);
	}
	~render()
	{
		//conio::set_cursor_visible(h_screen_buffer_, true);
		::SetConsoleActiveScreenBuffer(h_std_out_);
	}
	void set_char(int x, int y, WCHAR ch, WORD attr = FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY)
	{
		buff_[y * screen_size_.X + x] = { ch, attr };
	}
	void update()
	{
		SMALL_RECT rect{0, 0, screen_size_.X - 1, screen_size_.Y - 1 };
		::WriteConsoleOutputW(h_screen_buffer_, buff_.get(), screen_size_, COORD{0, 0}, &rect);
	}

	COORD get_screen_size()
	{
		return conio::get_screen_size(h_std_out_);
	}

private:
	HANDLE h_screen_buffer_{ INVALID_HANDLE_VALUE };
	HANDLE h_std_out_{ INVALID_HANDLE_VALUE };
	std::unique_ptr<CHAR_INFO[]> buff_;
	COORD screen_size_{ 0, 0 };
};

class snake final : public game_engine::game<snake>
{
	friend class game_engine::game<snake>;
	enum class direction
	{
		STOP,
		LEFT,
		RIGHT,
		UP,
		DOWN
	};

public:
	int run()
	{
		constexpr short k = 2;
		const auto screen_size = render_.get_screen_size();
		field_.X = std::min(screen_size.X, field_.X);
		field_.Y = std::min(screen_size.Y, field_.Y);
		const COORD head{ field_.X / k, field_.Y / k };
		tail_.emplace_back(head);
		tail_.emplace_back(head); // clean for the last
		update_fruit();
		draw_frame();
		game_engine::game<snake>::set_sleep_time(200);
		game_engine::game<snake>::run();
		return score_;
	}
private:
	void draw_frame()
	{
		for (int n = 0, y = 0; n < 2; ++n, y += (field_.Y + 1))
			for (int x = 0; x < (field_.X + 2); ++x)
				render_.set_char(x, y, '#');
		for (int n = 0, x = 0; n < 2; ++n, x += (field_.X + 1))
			for (int y = 0; y < (field_.Y + 2); ++y)
				render_.set_char(x, y, '#');
	}
	void draw_tail()
	{
		for (auto it = tail_.begin(), back = --tail_.end(); it != back; ++it)
			render_.set_char(it->X, it->Y, 'O');
	}
	void on_draw()
	{
		// clean
		auto& tail_back = tail_.back();
		render_.set_char(tail_back.X, tail_back.Y, 0, 0);

		draw_tail();		
		// fruit
		render_.set_char(fruit_.X, fruit_.Y, 'F');

		render_.update();
	}
	void on_input(int vkey)
	{
		switch (vkey)
		{
		case 0x250000:
		case 'a':
			if (direct_ != direction::RIGHT)
				direct_ = direction::LEFT;
			break;
		case 0x270000:
		case 'd':
			if (direct_ != direction::LEFT)
				direct_ = direction::RIGHT;
			break;
		case 0x260000:
		case 'w':
			if (direct_ != direction::DOWN)
				direct_ = direction::UP;
			break;
		case 0x280000:
		case 's':
			if (direct_ != direction::UP)
				direct_ = direction::DOWN;
			break;
		case 0x1b:
		case 'x':
			stop();
			break;
		}
	}
	void on_logic()
	{
		auto head = *tail_.begin();
		auto tail_last = --tail_.end();

		switch (direct_)
		{
		case direction::LEFT:
			--head.X;
			break;
		case direction::RIGHT:
			++head.X;
			break;
		case direction::UP:
			--head.Y;
			break;
		case direction::DOWN:
			++head.Y;
			break;
		default:
			break;
		}

		auto it = (direct_ != direction::STOP) ? 
			std::find_if(++tail_.begin(), tail_last, [&head](const COORD& t) {
				return t.X == head.X && t.Y == head.Y;
			}) : tail_last;

		if (it != tail_last || 
			head.X <= 0 || head.X > field_.X ||
			head.Y <= 0 || head.Y > field_.Y)
		{
			stop(); // game over
		}
		else
		{
			if (head.X == fruit_.X && head.Y == fruit_.Y)
			{
				tail_.emplace_front(head);
				++score_;
				fruit_live_time_ = 0;
			}
			else if (tail_.size() > 2)
			{
				tail_.splice(tail_.begin(), tail_, tail_last);
				tail_.front() = head;
			}
			else
			{
				tail_.back() = tail_.front();
				tail_.front() = head;
			}
			update_fruit();
		}
	}
	void update_fruit()
	{
		constexpr int fruit_live_time = 50;
		if (fruit_live_time_ == 0)
		{
			render_.set_char(fruit_.X, fruit_.Y, 0);
			auto tail_last = --tail_.end();
			auto& fruit = this->fruit_;
			do
			{
				fruit_ = COORD{ random_.get(3, field_.X - 3), random_.get(3, field_.Y - 3) };
			} while (std::find_if(tail_.begin(), tail_last, [&fruit](const COORD& t) {
				return t.X == fruit.X && t.Y == fruit.Y;
				}) != tail_last);
			fruit_live_time_ = fruit_live_time;
		}
		else
			--fruit_live_time_;
	}
private:
	static constexpr short kx = 30;
	static constexpr short ky = 20;
	COORD field_{ kx, ky };
	COORD fruit_{ -1, -1 };
	int fruit_live_time_{ 0 };
	int score_{ 0 };
	std::list<COORD> tail_;
	direction direct_{ direction::STOP };
	render render_{ kx + 2, ky + 2 };
	random_int<short> random_;
};

int main()
{
	snake instance;
	auto score = instance.run();
	std::cout << "Score: " << score << std::endl;
	return 0;
}
