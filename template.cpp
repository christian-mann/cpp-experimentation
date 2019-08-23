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

template <int Big, int Small>
struct mymodulus {
	static_assert(Small != 0, "Tried to take something mod 0");
	static const int value = Big % Small;
};

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

template <typename... Args>
struct RandomTypeSelector {
	static const int N = sizeof...(Args);
	using type = typename TypeSelector<mymodulus<TIMEINT, N>::value, Args...>::type;
};

template <template<typename...> class Continuation,
		 typename Devil,
		 typename Head,
		 typename... Tail>
struct Filter;

template <template <typename...> class Continuation,
		 typename Devil,
		 typename... Tail>
struct Filter<Continuation, Devil, Devil,
	Tail...>
{
	using type = typename Filter<Continuation, Devil, Tail...>::type;
};

template <template <typename...> class Continuation,
		 typename Devil,
		 typename Head,
		 typename... Tail>
struct Filter
{
	template <typename... Elements>
	using SubCont = Continuation<Head, Elements...>;

	using type = typename Filter<SubCont, Devil, Tail...>::type;
};

template <template <typename...> class Continuation,
		 typename Devil>
struct Filter<Continuation, Devil, Devil> {
	using type = Continuation<>;
};

template <template <typename...> class Continuation,
		 typename Devil,
		 typename Head>
struct Filter<Continuation, Devil, Head> {
	using type = Continuation<Head>;
};

/* Not sure how to make this work with empty list of types
 * The below is my best guess so far but it doesn't work
template <template <typename...> class Continuation,
		 typename Devil>
struct Filter<Continuation, Devil, void> {
	using type = Continuation<>;
}
Filter<tuple, float>::type tup2;
 */

/* This creates a tuple<int, double> because floats are removed from the list */
Filter<tuple, float, float, float, float, int, double, float>::type tup;

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
