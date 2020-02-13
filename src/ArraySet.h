#pragma once

#include "Array.h"
#include "Basic.h"

template <class ArrayType, typename T>
class _ArraySet {
public:
	ArrayType array;

	T &first() {
		return array[0];
	}

	_ArraySet() {}
	_ArraySet(std::initializer_list<T> init) : array(init) {}

	bool empty() const {
		return array.count == 0;
	}

	bool contains(const T &value) const {
		for (const T &item : array) {
			if (item == value)
				return true;
		}

		return false;
	}

	const T *begin() const {
		return array.begin();
	}

	const T *end() const {
		return array.end();
	}

	const T &operator[](u64 index) {
		return array[index];
	}

	const u32 size() const {
		return array.size();
	}

	void resize(u32 size) {
		array.resize();
	}

	void clear() {
		array.clear();
	}

	bool add(const T &value) {
		if (!contains(value)) {
			array.add(value);
			return true;
		}

		return false;
	}

	u32 addAndGetIndex(const T &value) {
		for (u32 i = 0; i < array.size(); i++)
			if (array[i] == value)
				return i;

		u32 result = array.size();
		array.add(value);
		return result;
	}

	bool remove(const T &value) {
		return array.unordered_remove(value);
	}


	void removeIndex(u32 index) {
		array.unordered_remove(&array[index]);
	}

	template <typename Other>
	bool operator==(const _ArraySet<Other, T> &other) {
		if (other.size() != size())
			return false;

		return containsAll(other);
	}


	template <typename Other>
	bool containsAll(const _ArraySet<Other, T> &other) {
		for (const T &o : other) {
			if (!contains(o)) return false;
		}

		return true;
	}

	template <typename Other>
	bool addAll(const _ArraySet<Other, T> &other) {
		bool change = false;

		for (const T &o : other) {
			change |= add(o);
		}

		return change;
	}


	template <typename Other>
	bool retainAll(const _ArraySet<Other, T> &other) {
		bool change = false;

		for (u32 i = 0; i < array.size(); i++) {
			if (!other.contains(array[i])) {
				array.unordered_remove(i);
				--i;
				change = true;
			}
		}

		return true;
	}

	template <typename Other>
	bool removeAll(const _ArraySet<Other, T> &other) {
		bool change = false;

		for (const T &o : other) {
			if (remove(o)) {
				if (empty())
					return true;

				change = true;
			}
		}

		return change;
	}

	template <typename Other>
	_ArraySet(const _ArraySet<Other, T> &other) : array(other.size()) {
		memcpy(array.storage, other.storage, sizeof(T) * array.size());
	}

	void free() {
		array.free();
	}

};

template <typename T>
using ArraySet = _ArraySet<Array<T>, T>;

template <typename T, int smallSize>
using SmallArraySet = _ArraySet<SmallArray<T, smallSize>, T>;