fn add(x, y) {
    int a, r;

    a = y;
    a++;
    if (a > 0) {
        loop(y)
            x++;
        r = x;
    } else {
        y = -y;
        loop(y)
            x--;
        r = x;
    }

    return(r);
}