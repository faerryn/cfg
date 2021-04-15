local packs = {}
local packs_install_path = vim.fn.stdpath"data".."/site/pack/use/opt/"

local function parse_packadd(args)
	local pack

	if type(args) == "string" then
		pack = packs[args]
	elseif type(args) == "table" then
		pack = args
	end
	if not pack then
		error("TODO")
	end

	return pack
end

local function packadd(args)
	local pack = parse_packadd(args)
	vim.api.nvim_command("packadd "..pack.short_install_path)
	if pack.config then pack.config() end
end

local function parse_use(args)
	local pack = {}

	if type(args) == "string" then
		pack.short_url = args
	elseif type(args) == "table" then
		pack.short_url = args[1]

		pack.init = args.init
		pack.config = args.config

		pack.disabled = args.disabled

		pack.after = args.after
		pack.defer = args.defer
	else
		error("TODO")
	end

	pack.url = "https://github.com/"..pack.short_url..".git"

	local match = pack.short_url:gmatch("[^/]+")
	pack.author = match()
	pack.name = match()
	pack.short_install_path = pack.author.."-"..pack.name
	pack.install_path = packs_install_path..pack.short_install_path

	pack.jobs = {}

	return pack
end

local Deque = require'deque'
packadd_remaining = Deque:new()
packadd_completed = {}

local function can_packadd(pack)
	if not pack.after then
		return true
	elseif type(pack.after) == "string" then
		return packadd_completed[pack.after]
	elseif type(pack.after) == "table" then
		for _, after in ipairs(pack.after) do
			if not packadd_completed[after] then
				return false
			end
		end
		return true
	else
		error("TODO")
	end
end

local function packadds()
	local counter = 0
	while packadd_remaining:len() > 0 do
		if counter >= packadd_remaining:len() then
			break
		end

		local pack = packadd_remaining:pop_front()

		if can_packadd(pack) then
			packadd(pack)
			packadd_completed[pack.short_url] = true
			counter = 0
		else
			packadd_remaining:push_back(pack)
			counter = counter + 1
		end
	end
end

local function use(args)
	local pack = parse_use(args)
	if pack.disabled then return end
	packs[pack.short_url] = pack
	if pack.init then pack.init() end

	if vim.fn.empty(vim.fn.glob(pack.install_path)) > 0 then
		pack.jobs.install = io.popen([[git clone --depth 1 --recurse-submodules ']]..pack.url..[[' ']]..pack.install_path..[[']], "r")
		if vim.v.vim_did_enter > 0 then
			pack.jobs.install:close()
			pack.jobs.install = nil
		end
	end

	packadd_remaining:push_back(pack)

	if vim.v.vim_did_enter > 0 then
		packadds()
	end
end

local function update()
	for _, pack in pairs(packs) do
		pack.jobs.update = io.popen([[git -C ']]..pack.install_path..[[' pull]], "r")
	end
	for _, pack in pairs(packs) do
		pack.jobs.update:close()
		pack.jobs.update = nil
	end
end

local function clean()
	local paths = {}

	for path in vim.fn.glob(vim.fn.resolve(packs_install_path.."/*")):gmatch("[^\n]+") do
		paths[path] = true
	end
	for _, pack in pairs(packs) do
		paths[pack.install_path] = false
	end

	for path, to_remove in pairs(paths) do
		if to_remove then
			os.execute("rm -rf "..path)
		end
	end
end

local function setup()
	if vim.v.vim_did_enter > 0 then
		for _, pack in pairs(packs) do
			if pack.jobs.install then
				pack.jobs.install:close()
				pack.jobs.install = nil
			end
		end
		packadds()
	else
		vim.api.nvim_command([[autocmd VimEnter * ++once lua require("user").setup()]])
	end
end

return {
	setup = setup,

	use = use,

	update = update,
	clean = clean,
}
