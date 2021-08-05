--- init.lua

-- bootstrap user.nvim
local user_packadd_path = "faerryn/user.nvim/default"
local user_install_path = vim.fn.stdpath("data").."/site/pack/user/opt/"..user_packadd_path
if vim.fn.isdirectory(user_install_path) == 0 then
  os.execute("git clone --quiet --depth 1 https://github.com/faerryn/user.nvim.git "..vim.fn.shellescape(user_install_path))
end
vim.api.nvim_command("packadd "..vim.fn.fnameescape(user_packadd_path))

local user = require("user")
user.setup()
local use = user.use

-- fennel for configuration
local function build_fennel()
  os.execute("make LUA=luajit PREFIX=./ BIN_DIR=./bin LUA_LIB_DIR=./lua install")
end
local fennel_pack = use {
  "fennel",
  repo = "https://git.sr.ht/~technomancy/fennel",
  install = build_fennel,
  update = build_fennel,
}
local fennel = require("fennel")

-- compile config.fnl
local config_fnl = vim.fn.stdpath("config").."/config.fnl"
local config_lua = vim.fn.stdpath("config").."/config.lua"
if vim.fn.getftime(config_fnl) > vim.fn.getftime(config_lua) then
  local fin = io.open(config_fnl, "r")
  local fennel_code = fin:read("*all")
  fin:close()
  
  local lua_code = fennel.compileString(fennel_code)

  local fout = io.open(config_lua, "w")
  fout:write(lua_code)
  fout:close()
end

-- load resulting config.lua
dofile(config_lua)
