#include <stdlib.h>
#include <random>
#include <vector>
#include <set>
#include <iostream>

using namespace std;

template <typename T, typename Alias>
Alias& create () {
	T* obj = new T();
	return (Alias&) *obj;
}

template <typename Base, typename... Derived>
struct RandomCreator {
	Base& createRandom() {
		std::vector<Base& (*)()> creators = {
			&create<Derived, Base>...
		};

		return creators [ rand() % creators.size() ]();
	}
};

#define TIMEsINT(s) \
	(int) (s[0] * 10 + s[1]) * 60 * 60 + \
	(int) (s[3] * 10 + s[4]) * 60 + \
	(int) (s[6] * 10 + s[7])
#define TIMEINT (TIMEsINT(__TIME__) + __LINE__)

template <typename T, int which, T... args>
struct Selector;

template <typename T, int which, T head, T... tail>
struct Selector<T, which, head, tail...> {
	static const T value = Selector<T, which-1, tail...>::value;
};

template <typename T, T head, T... tail>
struct Selector<T, 0, head, tail...> {
	static const T value = head;
};

template <typename T, T... Args>
struct RandomSelector {
	static const int N = sizeof...(Args);
	using type = typename Selector<T, mymodulus<TIMEINT, N>::value, Args...>::type;
};

template <int which, typename... args>
struct TypeSelector;

template <int which, typename head, typename... tail>
struct TypeSelector<which, head, tail...> {
	using type = typename TypeSelector<which-1, tail...>::type;
};

template <typename head, typename... tail>
struct TypeSelector<0, head, tail...> {
	using type = head;
};

template <int Big, int Small>
struct mymodulus {
	static_assert(Small != 0, "Tried to take something mod 0");
	static const int value = Big % Small;
};

template <typename... Args>
struct RandomTypeSelector {
	static const int N = sizeof...(Args);
	using type = typename TypeSelector<mymodulus<TIMEINT, N>::value, Args...>::type;
};

template <template <typename...> Continuation, typename Devil, typename... Elements>
struct Filter {
};

template <template <typename...> Continuation, typename Devil, typename Head, typename... Tail>
using Filter<Continuation, Devil, Head, Tail...> = Filter<Continuation, Devil, Head, Tail...>;

template <template <typename...> Continuation, typename Devil, typename... Tail>
using Filter<Continuation, Devil, Devil, Tail...> = Filter<Continuation, Devil, Tail...>;

template <template <typename...> Continuation, typename Devil, typename Head>
using Filter<Continuation, Devil, Head, Tail...> = Filter<Continuation, Devil, Tail...>;

template <template <typename...> Continuation, typename Devil, typename Head>
using Filter<Continuation, Devil, Head, Tail...> = Filter<Continuation, Devil, Tail...>;

struct Base {
	virtual string getName() { return "Base"; }
};
struct Derived1 : Base {
	virtual string getName() { return "Derived1"; }
};
struct Derived2 : Base {
	virtual string getName() { return "Derived2"; }
};

int main() {
	srand(time(NULL));

	cout << Selector<int, 3, 0, 1, 2, 3, 4>::value << "\n";

	RandomTypeSelector<int, float, double, long, long long>::type obj;
	RandomTypeSelector<int, float, double, long, long long>::type obj2;

	cout << obj << "\n";
	cout << obj2 << "\n";
}
