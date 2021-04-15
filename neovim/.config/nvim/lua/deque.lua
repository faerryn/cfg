Deque = {}

function Deque:new(deque)
	local deque = deque or {
		list = {},
		first = 1,
		last = 0,
	}
	setmetatable(deque, self)
	self.__index = self
	return deque
end

function Deque:len()
	return self.last + 1 - self.first
end

function Deque:front()
	return self.list[self.first]
end

function Deque:back()
	return self.list[self.last]
end

function Deque:pop_front()
	local value = self:front()
	self.list[self.first] = nil
	self.first = self.first + 1
	return value
end

function Deque:pop_back()
	local value = self:back()
	self.list[self.back] = nil
	self.last = self.last - 1
	return value
end

function Deque:push_front(value)
	self.first = self.first - 1
	self.list[self.first] = value
end

function Deque:push_back(value)
	self.last = self.last + 1
	self.list[self.last] = value
end

return Deque
