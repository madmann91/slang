void control_flow() {
    bool a, b, c, d;
    int i, j;

    if (a) {
    } else {
    }

    if (b) i++; else j++;

    if (c) i++;

    if (d) {}

    for (int i = 0; i < 3; i++) {
    }

    for (; i < 3; i++) {
    }

    for (;; i++) {
        if (i < 3) break;
    }

    for (;i < 3;) {
        if (i > 3) continue;
    }

    for (int i;;) {
        return;
    }

    discard;

    while (i < 5) {
        i++;
    }

    while (i < 5) i++;

    do { i++; } while (i < 5);
    do i++; while (i < 5);
}
