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
  vim.fn.mkdir("lua/", "p")
  if vim.fn.filereadable("lua/fennel.lua") then
    vim.fn.delete("lua/fennel.lua")
  end
  os.execute("make LUA=luajit")
  os.execute("cp fennel.lua lua/fennel.lua")
end
use {
  "fennel",
  repo = "https://git.sr.ht/~technomancy/fennel",
  install = build_fennel,
  update = build_fennel,
}
local fennel = require("fennel")
fennel.dofile(vim.fn.stdpath("config").."/init.fnl")
