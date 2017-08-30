import time
import logging
import sys
import json

import conf

from twisted.internet.protocol import ServerFactory, ClientFactory
from twisted.internet import reactor, protocol
from twisted.protocols.basic import LineReceiver
from twisted.web.client import getPage
from twisted.python import log
from twisted.application import service, internet

# Google Place API settings
GOOGLE_PLACE_API_ADDRESS = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"

#neighboring servers
NEIGHBORS = {
	"Alford" : ["Hamilton", "Welsh"],
	"Hamilton" : ["Holiday", "Alford"],
	"Welsh" : ["Alford", "Ball"],
	"Ball" : ["Holiday", "Welsh"],
	"Holiday" : ["Ball", "Hamilton"]
}

class ServerHerdProtocol(LineReceiver):
	def __init__(self,factory):
		self.factory = factory

	def connectionMade(self):
		self.factory.connections_num += 1
		logging.info("Connection has been established. Total number of connections is: {0}".format(self.factory.connections_num))

	def connectionLost(self, reason):
		self.factory.connections_num -= 1
		logging.info("Connection lost!")	

	def lineReceived(self, line):
		logging.info("Number of message received: {0}".format(line))
		command = line.split(" ")
        
        #dispatching the command to the corresponding handler
		if (command[0] == "IAMAT"):
			self.process_IAMAT(line)
		elif (command[0] == "WHATSAT"):
			self.process_WHATSAT(line)
		elif (command[0] == "AT"):
			self.process_AT(line)
		else:
			self.process_failure(line)
		return

    #deal with the error cases
	def process_failure(self, message):
		logging.info("Invalid command: " + message)
		self.transport.write("? " + message + "\n")
		return

	#IAMAT handler
	def process_IAMAT(self, message):
		command = message.split(" ")
		#check if the command is a valid one
		if len(command) != 4:
			self.process_failure(message)
			return
		client_id = command[1]
		client_loc = command[2]
		client_time = float(command[3])
		time_diff = time.time() - client_time

		if (time_diff >= 0):
			output = "+" + str(time_diff)
		else:
			output = str(time_diff)

		if client_id in self.factory.clients:
			logging.info("Loading from existing client: {0}".format(client_id))
		else:
			logging.info("Creating a new client id: {0}".format(client_id))
		
		#replace the "IAMAT" with "AT"
		command[0] = "AT"
		response = command.insert(1, output)
		response = command.insert(1, self.factory.server_name)
		response = ' '.join(command) + '\n'
		self.transport.write(response)

		logging.info("Sending location information to server neighbors")
		self.location_update(output,client_loc,client_time,client_id)

	#WHATSAT handler
	def process_WHATSAT(self,message):
		command = message.split()

		#check the command
		if len(command) != 4:
			self.process_failure(message)
			return

		client_id = command[1]
		radius = int(command[2])
		ubound = int(command[3])

		#check if the radius and ubound reach the limit
		if (radius > 50 or ubound > 20):
			self.process_failure(message)

		if not (client_id in self.factory.clients):
			self.process_failure(message)
			return

		#generate the client location in the output message
		client_loc = self.factory.clients[client_id]['location']
		new_loc = client_loc.split(' ')
		del new_loc[0]
		client_loc = client_loc.replace('+', ' +')
		client_loc = client_loc.replace('-',' -')
		client_loc = ','.join(new_loc)
		client_loc = client_loc.replace('+','')

		radius_m = str(int(float(radius)))
		query_request = GOOGLE_PLACE_API_ADDRESS + "location=" + client_loc + "&radius=" + radius_m + "&key=" + conf.API_KEY
		query_response = getPage(query_request)
		query_response.addCallback(callback = lambda x:(self.process_Google(x,ubound,client_id)))

	#printout the json format information
	def process_Google(self, query_response, ubound, client_id):
		logging.info("Google Place API query response: " + query_response)
		response_object = json.loads(query_response)
		response_object["results"] = response_object["results"][0:ubound]
		output = self.factory.clients[client_id]['message']+json.dumps(response_object,indent=4)+'\n\n'
		self.transport.write(output)

	#AT handler
	def process_AT(self,message):
		command = message.split(" ")
		#check the command
		if len(command) != 6:
			self.process_failure(message)
			return
		client_id = command[3]
		client_loc = command[4]
		client_time = command[5]

		if (client_id in self.factory.clients) and (client_time <= self.factory.clients[client_id]["time"]):
			logging.info("Wrong AT input " + message)
			return

		if client_id in self.factory.clients:
			logging.info("Loading location from existing client: {0}".format(client_id))
		else:
			logging.info("Creating a new client id: {0}".format(client_id))
		self.factory.clients[client_id] = {"Response from the server": ' '.join(command[:-1]),"time": client_time}
		self.location_update(self.factory.clients[client_id]["message"],client_loc,client_time,client_id)
		return

	#propagate information between servers
	def location_update(self,message,client_loc,client_time,client_id):
		self.factory.clients[client_id] = {
		'message': message,
		'location': client_loc,
		'time': client_time
		}
		for neighbor in NEIGHBORS[self.factory.server_name]:
                        reactor.connectTCP('localhost', conf.PORT_NUM[neighbor],ClientHerd(message))
			logging.info("Location update is sent from {0} to {1}".format(self.factory.server_name,neighbor))
		return

class ServerHerd(ServerFactory):
	def __init__(self,server_name):
		self.server_name = server_name
                self.port_number = conf.PORT_NUM[self.server_name]
		self.clients = {}
		self.connections_num = 0

	def buildProtocol(self,addr):
		return ServerHerdProtocol(self)

class ClientHerdProtocol(LineReceiver):
	def __init__(self,factory):
		self.factory = factory

	def connectionMade(self):
		self.sendLine(self.factory.message)

class ClientHerd(protocol.ClientFactory):
	def __init__(self,message):
		self.message = message

	def buildProtocol(self,addr):
		return ClientHerdProtocol(self)

def main():
	if len(sys.argv) != 2:
		print "Incorrect number of arguments"
		exit()
	server_name = sys.argv[1]
	try:
                if server_name in conf.PORT_NUM:
			factory = ServerHerd(server_name)
                        reactor.listenTCP(conf.PORT_NUM[server_name],factory)
			reactor.run()
		else:
			print "Error: Server name does not match with our record"
	except KeyError:
		print "Error: unexpected arguments"

if __name__ == '__main__':
    main()
