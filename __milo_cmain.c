
extern int __program_start(void);

int main(int argc, char **argv) {
	return __program_start();
}

int WinMain(void *hInstance, void *hPrevInstance, char *lpCmdLine, int nShowCmd) {
	return __program_start();
}