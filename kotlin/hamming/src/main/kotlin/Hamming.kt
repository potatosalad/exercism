package Hamming

fun compute(s1: String, s2: String): Int {
    require (s1.length == s2.length) {
        "left and right strands must be of equal length."
    }
    return s1.zip(s2).filter({ (c1, c2) -> c1 != c2 }).count()
}
