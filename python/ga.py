import numpy as np
import random as rand

bigG = 6.673e-11

# lets represent a deposit as a numpy array of parameters
class Deposit(object):

  def __init__(self, x, z, r, p):
    self.params = np.array([x, z, r, p])

  def volume(self):
    return (4.0/3) * np.pi * self.params[2] ** 3

  def mass(self):
    return self.params[3] * self.volume()

  def grav(self, point):
    x, z = point
    sx, sz = self.params[:2]
    d = np.sqrt((sx - x) ** 2 + (sz - z) ** 2)
    k = bigG * self.mass() / (d ** 3)
    return k * np.array([sx - x, sz - z])

  def translate(self):
    nx = rand.uniform(-50.0,50.0)
    nz = rand.uniform(-50.0,50.0)
    x, z, r, p = self.params
    return Deposit(x + nx, z + nz, r, p)

  def scale(self):
    k = rand.uniform(0.0,2.0)
    x, z, r, p = self.params
    return Deposit(x, z, k*r, p)



# okay now we need to be able to produce a random deposit
def randomDeposit():
  r = rand.uniform(0.0, 1000.0)
  x = rand.uniform(500.0, 1500.0)
  z = rand.uniform(0, -1000.0)
  return Deposit(x, z, r, 2500.0)

# okay now lets store the flyover data in a numpy array [x, z, gx, gz]
flyover = np.array([
  [0.0, 0.0, 7.81276478e-06, -3.90638239e-06],
  [1000.0, 0.0, 0.0, -4.36746829e-05],
  [1500.0, 200.0, -8.57614655e-06, -1.20066052e-05]])

sampleDeposit = Deposit(1000.0, -500.0, 250.0, 2500.0)

# the fitness function. I know there is much better ways to do this.
def fitness(dep):
  error = 0.0
  for fly in flyover:
    error += np.sum((dep.grav(fly[:2]) - fly[2:4]) ** 2)
  return error

# function for creating new mutants
def evolve(dep):
  r = rand.random()
  if r < 0.2:
    return dep.translate()
  elif r < 0.4:
    return dep.scale()
  else:
    return None


if __name__ == "__main__":

  rand.seed()

  pop = [randomDeposit() for i in range(100)]

  for i in range(1000):
    pop.sort(key=fitness)
    pop = pop[:100]
    newbies = [n for n in [evolve(p) for p in pop] if n is not None]
    pop.extend(newbies)

  pop.sort(key=fitness)


  

