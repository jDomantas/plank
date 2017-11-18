pub mod liveness;

use ir::BlockId;


#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub struct Loc {
    pub block: BlockId,
    pub pos: usize,
}
