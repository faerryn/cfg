"" init.vim

if has("nvim-0.5.0")
	execute "luafile" stdpath("config").."/nightly.lua"
else
	execute "source" stdpath("config").."/stable.vim"
end
