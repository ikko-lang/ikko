type Move enum:
    Rock
    Paper
    Scissors

type Beats class:
    fn beats(Self, Self) Bool

instance Beats Move:
    fn beats(self, other):
        match self:
            Rock():
                return other == Scissors{}
            Paper():
                return other == Rock{}
            Scissors():
                return other == Paper{}

fn main():
    let result = beats(Rock{}, Scissors{})
    print("\n")
