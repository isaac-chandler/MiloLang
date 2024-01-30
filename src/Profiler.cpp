#include "Basic.h"
#include "Profiler.h"
#include "Array.h"
#include "BucketedArenaAllocator.h"

static volatile Profile **allocated_profiles;
static volatile u64 g_max_profiler_threads;
static u64 g_max_profiles_per_thread;
static std::atomic_int32_t thread_index;

static u64 start_tsc = __rdtsc();
static u64 start_time = performance_time();

#if BUILD_PROFILE

void profiler_init(u64 max_profiler_threads, u64 max_zones_per_thread) {

	g_max_profiler_threads = max_profiler_threads;
	g_max_profiles_per_thread = max_zones_per_thread * 2;
	allocated_profiles = new volatile Profile * [max_profiler_threads];
	
	for (u64 i = 0; i < max_profiler_threads; i++) {
		allocated_profiles[i] = new Profile[g_max_profiles_per_thread];
		memset((void *)allocated_profiles[i], 0, sizeof(Profile) * g_max_profiles_per_thread);
	}

	read_write_barrier();
	start_tsc = __rdtsc();
	start_time = performance_time();
}

void profiler_register_this_thread() {
	s32 index = thread_index.fetch_add(1, std::memory_order_relaxed);

	if (index >= g_max_profiler_threads) {
		fprintf(stderr, "Profiler Error: Maximum number of threads (%" PRIu64 ") exceeded", g_max_profiler_threads);
		exit(1);
	}

	profile_index = allocated_profiles[index];
}

void profiler_write_tracing_json(const char *filename, double timescale) {
	printf("Writing profiles to %s\n", filename);
	FILE *out = fopen(filename, "wb");

	u64 pcf = performance_frequency();

	u64 end_tsc = __rdtsc();

	u64 end_time = performance_time();

	double tsc_factor = (1e6 * timescale) * (double) (end_time - start_time) / (double) (end_tsc - start_tsc) / (double) pcf;

	fputc('[', out);
	bool first = true;

	u64 high_water_mark = 0;
	u64 thread_count = thread_index.load(std::memory_order_relaxed);
	u64 total_profiles = 0;

	for (u64 thread_id = 0; thread_id < thread_count; thread_id++) {
		u64 i = 0;
		for (; i < g_max_profiles_per_thread; i++) {
			volatile Profile *p = &allocated_profiles[thread_id][i];
			if (!p->time)
				break;
			++total_profiles;

			if (!first) {
				fputs(",\n", out);
			}
			first = false;

			fprintf(out, "{\"cat\":\"function\",\"pid\":0,\"tid\":%u,\"ts\":%llu", thread_id, (u64) ((p->time - start_tsc) * tsc_factor));

			if (p->name) {
				fputs(",\"ph\":\"B\",\"name\":\"", out);

				for (const char *name = p->name; *name; name++) {
					if (*name == '\\')
						fputs("\\\\", out);
					else if (*name == '\"')
						fputs("\\\"", out);
					else if (*name == '\n')
						fputs("\\n", out);
					else if (*name == '\r')
						fputs("\\r", out);
					else if (*name == '\t')
						fputs("\\t", out);
					else if (*name == '\f')
						fputs("\\f", out);
					else if (*name == '\b')
						fputs("\\b", out);
					else if (*name < 32)
						fprintf(out, "\\u%04x", *name);
					else
						fputc(*name, out);

				}

				fputs("\"}", out);
			}
			else {
				fputs(",\"ph\":\"E\"}", out);
			}
		}

		high_water_mark = std::max(high_water_mark, i);
	}

	fputc(']', out);
	fclose(out);

	printf("Wrote %llu zones from %llu threads\n", total_profiles / 2, thread_count);
	printf("High water mark: %llu%%\n", high_water_mark * 100 / g_max_profiles_per_thread);
}

void profiler_write_binary(const char *filename) {
	printf("Writing profiles to %s\n", filename);
	u64 pcf = performance_frequency();

	u64 end_tsc = __rdtsc();
	u64 end_time = performance_time();

	double tsc_factor = 1e9 * (double) (end_time - start_time) / (double) (end_tsc - start_tsc) / (double) pcf;

	u64 thread_count = (u64) thread_index.load(std::memory_order::relaxed);

	u64 *threads_counts = new u64[thread_count];

	u64 high_water_mark = 0;
	u64 total_profiles = 0;

	for (u64 thread_id = 0; thread_id < thread_count; thread_id++) {
		u64 i = 0;
		for (; i < g_max_profiles_per_thread; i++) {
			volatile Profile *p = &allocated_profiles[thread_id][i];
			if (!p->time)
				break;
			++total_profiles;
		}

		high_water_mark = std::max(high_water_mark, i);
		threads_counts[thread_id] = i;
	}

	BucketedArenaAllocator allocator(65536);
	allocator.ensure(24 + 16 * thread_count);
	allocator.addNullTerminatedString("mprof");
	allocator.add2Unchecked(1);
	u64 *threads_array_offset = allocator.add8Unchecked(0);
	allocator.add8Unchecked(thread_count);
	*threads_array_offset = allocator.totalSize;

	u64 *threads_offset = (u64 *)allocator.allocateUnaligned((u32) (2 * thread_count * sizeof(u64)));

	Array<u64 *> name_patches;


	for (u64 thread_id = 0; thread_id < thread_count; thread_id++) {
		threads_offset[2 * thread_id] = allocator.totalSize;
		threads_offset[2 * thread_id + 1] = threads_counts[thread_id];
		
		for (u64 i = 0; i < threads_counts[thread_id]; i++) {
			volatile Profile *p = &allocated_profiles[thread_id][i];
			if (!p->time)
				break;

			allocator.ensure(24);

			if (p->name) {
				name_patches.add(allocator.add8Unchecked(0));
				allocator.add8Unchecked(strlen(p->name));
			}
			else {
				allocator.add8Unchecked(0);
				allocator.add8Unchecked(0);
			}
			allocator.add8Unchecked((u64) ((p->time - start_tsc) * tsc_factor));
		}
	}

	u64 patch_index = 0;
	for (u64 thread_id = 0; thread_id < thread_count; thread_id++) {
		for (u64 i = 0; i < threads_counts[thread_id]; i++) {
			volatile Profile *p = &allocated_profiles[thread_id][i];
			if (!p->time)
				break;

			if (p->name) {
				// TODO: Merge identical strings
				*name_patches[patch_index] = allocator.totalSize;
				allocator.addString(p->name);
				patch_index++;
			}
		}
	}

	FILE *out = fopen(filename, "wb");
	allocator.writeToFile(out);
	fclose(out);
	allocator.free();
	name_patches.free();
	delete[] threads_counts;

	printf("Wrote %llu zones from %llu threads\n", total_profiles / 2, thread_count);
	printf("High water mark: %llu%%\n", high_water_mark * 100 / g_max_profiles_per_thread);
}

#endif