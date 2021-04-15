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
		pack.defer = args.defer or pack.after
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
	pack.active = false

	return pack
end

local function use(args)
	local pack = parse_use(args)
	if pack.disabled then return end
	packs[pack.short_url] = pack
	if pack.init then pack.init() end

	if (not pack.defer) and vim.v.vim_did_enter then
		packadd(pack)
	end
end

local function install()
	for _, pack in pairs(packs) do
		if vim.fn.empty(vim.fn.glob(pack.install_path)) > 0 then
			pack.jobs.install([[git clone --depth 1 --recurse-submodules ']]..pack.url..[[' ']]..pack.install_path..[[']])
		end
	end
	for _, pack in pairs(packs) do
		if pack.jobs.install then
			pack.jobs.install:close()
			pack.jobs.install = nil
		end
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
	if vim.v.vim_did_enter then
		install()

		for _, pack in pairs(packs) do
			if not pack.defer then
				packadd(pack)
			end
		end
	else
		vim.api.nvim_command([[autocmd VimEnter * ++once lua require("user").setup()]])
	end
end

return {
	setup = setup,

	packadd = packadd,
	use = use,

	install = install,
	update = update,
	clean = clean,
}
