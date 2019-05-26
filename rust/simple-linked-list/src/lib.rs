pub struct SimpleLinkedList<T> {
    head: Elem<T>,
}

type Elem<T> = Option<Box<Node<T>>>;

struct Node<T> {
    elem: T,
    next: Elem<T>,
}

impl<T> SimpleLinkedList<T> {
    pub fn new() -> Self {
        SimpleLinkedList { head: None }
    }

    pub fn len(&self) -> usize {
        let mut size = 0_usize;
        let mut tail = &self.head;
        while let Some(node) = tail {
            size += 1;
            tail = &node.next;
        }
        size
    }

    pub fn push(&mut self, element: T) {
        self.head = Some(Box::new(Node {
            elem: element,
            next: self.head.take(),
        }));
    }

    pub fn pop(&mut self) -> Option<T> {
        self.head.take().map(|node| {
            self.head = node.next;
            node.elem
        })
    }

    pub fn peek(&self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.elem)
    }
}

impl<T: Clone> SimpleLinkedList<T> {
    pub fn rev(&self) -> SimpleLinkedList<T> {
        let mut list = SimpleLinkedList::new();
        let mut tail = &self.head;
        while let Some(node) = tail {
            list.push(node.elem.clone());
            tail = &node.next;
        }
        list
    }
}

impl<'a, T: Clone> From<&'a [T]> for SimpleLinkedList<T> {
    fn from(item: &[T]) -> Self {
        let mut list = SimpleLinkedList::new();
        for i in item {
            list.push(i.clone());
        }
        list
    }
}

impl<T> Into<Vec<T>> for SimpleLinkedList<T> {
    fn into(self) -> Vec<T> {
        let mut vec = Vec::new();
        let mut tail = self.head;
        while let Some(node) = tail {
            vec.insert(0, node.elem);
            tail = node.next;
        }
        vec
    }
}
