import random

class agent:
    def __init__(self):
        self.vote = list()
        self.position = list()
        self.salience = list()
        self.domestic_pressure = list()

    def preference(self):
        position = (random.uniform(1,100),random.uniform(1,100))
        return position

    def importance(self):
        salience = (random.uniform(0,1),random.uniform(0,1))
        return salience

class simulation:
    def __init__(self, agents = 100):
        self.sq = (random.uniform(1,100),random.uniform(1,100))
        self.players = list()
        self.history = list()
        self.sq = list()

        for i in range(agents):
            self.players.append(agent())

    def run(self, iterations = 100):
        for play in range(iterations):
            sq = random.uniform(1,100)
            
            for player in self.players:
                player.position.append(player.preference())
                player.salience.append(player.importance())

            Pref = [x.position[play] for x in self.players]
            Sal = [x.salience[play] for x in self.players]
            outcome = sum(Pref)*sum(Sal)/sum(Pref)

            for player in self.players:
                utility1 = abs(sq - player.position[play])
                utility2 = abs(outcome - player.position[play])
                if utility1 > utility2:
                    player.vote.append("No")
                else:
                    player.vote.append("Yes")
                
                
                
            
            
        

        
        
    
