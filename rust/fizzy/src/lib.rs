pub type MatchFn<T> = Box<dyn Fn(T) -> bool>;

pub struct Matcher<T> {
    matcher: MatchFn<T>,
    subs: String,
}

impl<T> Matcher<T> {
    pub fn new<F, S>(matcher: F, subs: S) -> Matcher<T>
    where
        F: Fn(T) -> bool + 'static,
        S: AsRef<str>,
    {
        Matcher {
            matcher: Box::new(matcher),
            subs: subs.as_ref().to_string(),
        }
    }
}

#[derive(Default)]
pub struct Fizzy<T>(Vec<Matcher<T>>);

impl<T> Fizzy<T>
where
    T: Copy + ToString,
{
    pub fn new() -> Self {
        Fizzy(Vec::new())
    }

    pub fn add_matcher(mut self, matcher: Matcher<T>) -> Self {
        self.0.push(matcher);
        self
    }

    pub fn apply<I>(self, iter: I) -> impl Iterator<Item = String>
    where
        I: Iterator<Item = T>,
    {
        iter.map(move |item| self.apply_item(item))
    }

    fn apply_item(&self, item: T) -> String {
        let Fizzy(ref matchers) = self;
        let mut out = String::new();
        for matcher in matchers {
            if (matcher.matcher)(item) {
                out.push_str(&matcher.subs);
            }
        }
        if out.is_empty() {
            item.to_string()
        } else {
            out
        }
    }
}

pub fn fizz_buzz<T>() -> Fizzy<T>
where
    T: Copy + Default + From<u8> + PartialEq + std::ops::Rem<Output = T> + 'static,
{
    let three: T = 3.into();
    let five: T = 5.into();
    Fizzy(vec![
        Matcher::new(move |n| n % three == T::default(), "fizz"),
        Matcher::new(move |n| n % five == T::default(), "buzz"),
    ])
}
