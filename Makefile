debug:
	cmake -Bbuild_debug -GNinja -DCMAKE_BUILD_TYPE=Debug
	ninja -C build_debug
release:
	cmake -Bbuild_release -GNinja -DCMAKE_BUILD_TYPE=RelWithDebInfo 
	ninja -C build_release