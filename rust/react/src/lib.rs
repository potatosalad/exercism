use std::collections::HashMap;

/// `InputCellID` is a unique identifier for an input cell.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct InputCellID(usize);
/// `ComputeCellID` is a unique identifier for a compute cell.
/// Values of type `InputCellID` and `ComputeCellID` should not be mutually assignable,
/// demonstrated by the following tests:
///
/// ```compile_fail
/// let mut r = react::Reactor::new();
/// let input: react::ComputeCellID = r.create_input(111);
/// ```
///
/// ```compile_fail
/// let mut r = react::Reactor::new();
/// let input = r.create_input(111);
/// let compute: react::InputCellID = r.create_compute(&[react::CellID::Input(input)], |_| 222).unwrap();
/// ```
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ComputeCellID(usize);
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CallbackID(usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CellID {
    Input(InputCellID),
    Compute(ComputeCellID),
}

#[derive(Debug, PartialEq)]
pub enum RemoveCallbackError {
    NonexistentCell,
    NonexistentCallback,
}

struct InputCell<T> {
    value: T,
}

impl<T> InputCell<T> {
    fn new(value: T) -> Self {
        InputCell { value }
    }
}

struct ComputeCell<'cell, T: 'cell> {
    value: T,
    dependencies: Vec<CellID>,
    compute_func: Option<Box<Fn(&[T]) -> T>>,
    callbacks: HashMap<usize, Box<FnMut(T) -> () + 'cell>>,
}

impl<'cell, T> ComputeCell<'cell, T> {
    fn new(value: T, dependencies: &[CellID]) -> Self {
        ComputeCell {
            value,
            dependencies: dependencies.to_vec(),
            compute_func: None,
            callbacks: HashMap::new(),
        }
    }
}

enum Cell<'cell, T: 'cell> {
    Input(InputCell<T>),
    Compute(ComputeCell<'cell, T>),
}

pub struct Reactor<'reactor, T: 'reactor> {
    cells: Vec<Cell<'reactor, T>>,
    count: usize,
}

impl<'reactor, T: 'reactor> Default for Reactor<'reactor, T> {
    fn default() -> Self {
        Reactor {
            cells: Vec::new(),
            count: 0,
        }
    }
}

// You are guaranteed that Reactor will only be tested against types that are Copy + PartialEq.
impl<'reactor, T: Copy + PartialEq> Reactor<'reactor, T> {
    pub fn new() -> Self {
        Default::default()
    }

    // Creates an input cell with the specified initial value, returning its ID.
    pub fn create_input(&mut self, initial: T) -> InputCellID {
        self.cells.push(Cell::Input(InputCell::new(initial)));
        InputCellID(self.cells.len() - 1)
    }

    // Creates a compute cell with the specified dependencies and compute function.
    // The compute function is expected to take in its arguments in the same order as specified in
    // `dependencies`.
    // You do not need to reject compute functions that expect more arguments than there are
    // dependencies (how would you check for this, anyway?).
    //
    // If any dependency doesn't exist, returns an Err with that nonexistent dependency.
    // (If multiple dependencies do not exist, exactly which one is returned is not defined and
    // will not be tested)
    //
    // Notice that there is no way to *remove* a cell.
    // This means that you may assume, without checking, that if the dependencies exist at creation
    // time they will continue to exist as long as the Reactor exists.
    pub fn create_compute<F: Fn(&[T]) -> T + 'static>(
        &mut self,
        dependencies: &[CellID],
        compute_func: F,
    ) -> Result<ComputeCellID, CellID> {
        let compute_func = Box::new(compute_func);
        let value = self.compute_value(dependencies, &compute_func);
        let mut cell = ComputeCell::new(value?, dependencies);
        cell.compute_func = Some(compute_func);
        self.cells.push(Cell::Compute(cell));
        Ok(ComputeCellID(self.cells.len() - 1))
    }

    // Retrieves the current value of the cell, or None if the cell does not exist.
    //
    // You may wonder whether it is possible to implement `get(&self, id: CellID) -> Option<&Cell>`
    // and have a `value(&self)` method on `Cell`.
    //
    // It turns out this introduces a significant amount of extra complexity to this exercise.
    // We chose not to cover this here, since this exercise is probably enough work as-is.
    pub fn value(&self, id: CellID) -> Option<T> {
        let index = match id {
            CellID::Input(InputCellID(index)) | CellID::Compute(ComputeCellID(index)) => index,
        };
        self.cells.get(index).map(|cell| match cell {
            Cell::Input(c) => c.value,
            Cell::Compute(c) => c.value,
        })
    }

    // Sets the value of the specified input cell.
    //
    // Returns false if the cell does not exist.
    //
    // Similarly, you may wonder about `get_mut(&mut self, id: CellID) -> Option<&mut Cell>`, with
    // a `set_value(&mut self, new_value: T)` method on `Cell`.
    //
    // As before, that turned out to add too much extra complexity.
    pub fn set_value(&mut self, id: InputCellID, new_value: T) -> bool {
        if let Some(Cell::Input(cell)) = self.cells.get_mut(id.0) {
            cell.value = new_value;
            self.compute_values();
            true
        } else {
            false
        }
    }

    // Adds a callback to the specified compute cell.
    //
    // Returns the ID of the just-added callback, or None if the cell doesn't exist.
    //
    // Callbacks on input cells will not be tested.
    //
    // The semantics of callbacks (as will be tested):
    // For a single set_value call, each compute cell's callbacks should each be called:
    // * Zero times if the compute cell's value did not change as a result of the set_value call.
    // * Exactly once if the compute cell's value changed as a result of the set_value call.
    //   The value passed to the callback should be the final value of the compute cell after the
    //   set_value call.
    pub fn add_callback<F: FnMut(T) -> () + 'reactor>(
        &mut self,
        id: ComputeCellID,
        callback: F,
    ) -> Option<CallbackID> {
        if let Some(Cell::Compute(cell)) = self.cells.get_mut(id.0) {
            cell.callbacks.insert(self.count, Box::new(callback));
            self.count += 1;
            Some(CallbackID(self.count - 1))
        } else {
            None
        }
    }

    // Removes the specified callback, using an ID returned from add_callback.
    //
    // Returns an Err if either the cell or callback does not exist.
    //
    // A removed callback should no longer be called.
    pub fn remove_callback(
        &mut self,
        cell: ComputeCellID,
        callback: CallbackID,
    ) -> Result<(), RemoveCallbackError> {
        if let Some(Cell::Compute(cell)) = self.cells.get_mut(cell.0) {
            match cell.callbacks.remove(&callback.0) {
                Some(_) => Ok(()),
                None => Err(RemoveCallbackError::NonexistentCallback),
            }
        } else {
            Err(RemoveCallbackError::NonexistentCell)
        }
    }

    fn compute_value<F: Fn(&[T]) -> T>(
        &self,
        dependencies: &[CellID],
        compute_func: &F,
    ) -> Result<T, CellID> {
        let mut values = Vec::new();
        for &d in dependencies {
            if let Some(v) = self.value(d) {
                values.push(v);
            } else {
                return Err(d);
            }
        }
        Ok(compute_func(&values))
    }

    fn compute_values(&mut self) {
        for id in 0..self.cells.len() {
            if let Some(Cell::Compute(_)) = self.cells.get(id) {
                let compute_func;
                let dependencies;
                let new_value;
                {
                    let cell = if let Some(Cell::Compute(c)) = self.cells.get_mut(id) {
                        c
                    } else {
                        unreachable!()
                    };
                    compute_func = cell.compute_func.take();
                    dependencies = cell.dependencies.clone();
                }
                if let Some(compute_func) = compute_func {
                    let values = dependencies
                        .iter()
                        .map(|&d| self.value(d).unwrap())
                        .collect::<Vec<_>>();
                    new_value = compute_func(&values);
                    let mut cell = if let Some(Cell::Compute(c)) = self.cells.get_mut(id) {
                        c
                    } else {
                        unreachable!()
                    };
                    if new_value != cell.value {
                        for callback in cell.callbacks.values_mut() {
                            callback(new_value);
                        }
                        cell.value = new_value;
                    }
                    cell.compute_func = Some(compute_func);
                }
            }
        }
    }
}
