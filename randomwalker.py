#an 2d implementation of random walk
#implemented by: Sandeep Ghimire
#uses turtle graphics

import turtle
import random


class Walker(turtle.Turtle):
    """Walker takes the displacement in pixel to take in a single step"""
    def __init__(self,step, *args,**kwargs):
        super().__init__(*args,**kwargs)
        self.step = step
    
    def take_a_step(self):
        self.goto(self._next_position())
        
    def _next_position(self):
        random_step = random.choice([(-self.step,0),(self.step,0),(0,self.step),(0,-self.step)])
        return self.pos()[0]+random_step[0],self.pos()[1]+random_step[1]


def randomwalk(walker,dest):
    steps_taken = 0
    while walker.pos() != dest:
        walker.take_a_step()
        steps_taken += 1
    return steps_taken*walker.step,((dest[0]**2+dest[1]**2)**0.5)



def main():
    #speed up a bit
    #turtle.delay(0)
    turtle.tracer(100) # lower this value for fun animation but slower computations
    print(randomwalk(Walker(10),(100,100))) #change dest for different destination

if __name__ == "__main__":
    main()
