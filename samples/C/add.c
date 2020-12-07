fn add(x, y) {
    int a;

    a = y;
    a++;
    if (a > 0) {
        loop(y)
            x++;
        return(x);
    } else {
        y = -y;
        loop(y)
            x--;
        return(x);
    }
}