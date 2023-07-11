#pragma once

#include "Array.h"
#include "Basic.h"

template <typename T>
class ArraySet {
public:
	Array<T> array;

	T &first() {
		return array[0];
	}

	ArraySet() {}
	ArraySet(std::initializer_list<T> init) : array(init) {}

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

	T *begin() {
		return array.begin();
	}

	T *end() {
		return array.end();
	}

	const T *begin() const {
		return array.begin();
	}

	const T *end() const {
		return array.end();
	}

	const T &operator[](u32 index) const {
		return array[index];
	}

	T &operator[](u32 index) {
		return array[index];
	}

	u32 size() const {
		return array.count;
	}

	void resize(u32 size) {
		array.resize(size);
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

	bool operator==(const ArraySet<T> &other) {
		if (other.size() != size())
			return false;

		return containsAll(other);
	}


	bool containsAll(const ArraySet<T> &other) {
		for (const T &o : other) {
			if (!contains(o)) return false;
		}

		return true;
	}

	bool addAll(const ArraySet<T> &other) {
		bool change = false;

		for (const T &o : other) {
			change |= add(o);
		}

		return change;
	}


	bool retainAll(const ArraySet<T> &other) {
		bool change = false;

		for (u32 i = 0; i < array.size(); i++) {
			if (!other.contains(array[i])) {
				array.unordered_remove(i);
				--i;
				change = true;
			}
		}

		return change;
	}

	bool removeAll(const ArraySet<T> &other) {
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

	ArraySet(const ArraySet<T> &other) : array(other.array.count) {
		memcpy(array.storage, other.array.storage, sizeof(T) * array.count);
	}

	void free() {
		array.free();
	}

};