use std::ops::Add;

pub struct Triangle<T> {
    a: T,
    b: T,
    c: T,
}

impl<T> Triangle<T>
where
    T: PartialEq + PartialOrd + Copy + Add<T, Output = T> + From<T> + Into<f64>,
{
    pub fn build(sides: [T; 3]) -> Option<Self> {
        let [a, b, c] = sides;
        if a.into() > 0.0
            && b.into() > 0.0
            && c.into() > 0.0
            && a + b >= c
            && a + c >= b
            && b + c >= a
        {
            Some(Triangle { a, b, c })
        } else {
            None
        }
    }

    pub fn is_equilateral(&self) -> bool {
        self.a == self.b && self.b == self.c
    }

    pub fn is_scalene(&self) -> bool {
        self.a != self.b && self.a != self.c && self.b != self.c
    }

    pub fn is_isosceles(&self) -> bool {
        self.a == self.b || self.a == self.c || self.b == self.c
    }

    pub fn is_degenerate(&self) -> bool {
        self.a + self.b == self.c || self.a + self.c == self.b || self.b + self.c == self.a
    }

    pub fn area(&self) -> f64 {
        let (a, b, c) = (self.a.into(), self.b.into(), self.c.into());
        let p = self.perimeter();
        f64::sqrt(p * (p - a) * (p - b) * (p - c))
    }

    pub fn perimeter(&self) -> f64 {
        let (a, b, c) = (self.a.into(), self.b.into(), self.c.into());
        (a + b + c) / 2.0
    }
}

#[test]
fn degenerate_triangles_have_zero_area() {
    let sides = [1, 1, 2];
    let triangle = Triangle::build(sides).unwrap();
    assert!(!triangle.is_equilateral());
    assert!(triangle.is_isosceles());
    assert!(!triangle.is_scalene());
    assert!(triangle.is_degenerate());
    assert_eq!(triangle.area() as u64, 0_u64);
}
