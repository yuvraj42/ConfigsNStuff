-- ============================================================
-- lazy.nvim bootstrap: auto-installs the manager if absent
-- ============================================================
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- ============================================================
-- Plugins
-- ============================================================
require("lazy").setup({

  -- File finding
  {
    "nvim-telescope/telescope.nvim",          -- fuzzy finder (:Telescope find_files etc.)
    dependencies = { "nvim-lua/plenary.nvim" },
  },

  -- File browser
  "tpope/vim-vinegar",                        -- press - to open netrw; fixes papercuts

  -- Git
  "tpope/vim-fugitive",                       -- :Git, :Gdiffsplit, :Git blame

  -- Editing essentials
  "tpope/vim-commentary",                     -- gcc to comment line, gc + motion
  "tpope/vim-surround",                       -- cs"' ysiw] ds( etc.

  -- Completion
  "hrsh7th/nvim-cmp",                         -- completion engine
  "hrsh7th/cmp-nvim-lsp",                     -- LSP source for nvim-cmp
  "hrsh7th/cmp-buffer",                       -- buffer words source
  "hrsh7th/cmp-path",                         -- filesystem path source
  "L3MON4D3/LuaSnip",                         -- snippet engine (required by cmp)
  "saadparwaiz1/cmp_luasnip",                 -- snippet source for cmp

  -- Treesitter: better syntax highlighting and text objects for all languages
  -- replaces haskell-vim, python-syntax, etc. with one unified system
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    lazy = false,
    opts = {
      ensure_installed = { "python", "haskell", "lua", "vim", "json", "yaml", "markdown", "cpp" },
      auto_install     = true,
      highlight        = { enable = true },
      indent           = { enable = true },
    },
  },

  -- Indent guides (like highlight-indent-guides in emacs)
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    config = function() require("ibl").setup() end,
  },

  -- CSV
  "chrisbra/csv.vim",

  -- Markdown
  "preservim/vim-markdown",                   -- syntax, folding, :Toc
  {
    "iamcco/markdown-preview.nvim",           -- :MarkdownPreview live browser preview
    build = "cd app && npx --yes yarn install",
    ft = { "markdown" },
  },

})

-- ============================================================
-- Basic settings
-- (neovim already sets: nocompatible, syntax on, encoding=utf-8,
--  incsearch, and a handful of other sensible defaults)
-- ============================================================
local opt = vim.opt

-- Appearance
opt.number     = true
opt.cursorline = true
opt.scrolloff  = 8
opt.wrap       = true
opt.showmatch  = true
opt.title      = true

-- Indentation
opt.tabstop     = 4
opt.shiftwidth  = 4
opt.expandtab   = true
opt.smartindent = true

-- Search
opt.ignorecase = true
opt.smartcase  = true
opt.hlsearch   = true

-- File finding
opt.path:append("**")
opt.wildignore:append({ "*/node_modules/*", "*/.git/*", "*.pyc", "*/__pycache__/*" })

-- Usability
opt.mouse     = "a"
opt.clipboard = "unnamed"
opt.undofile  = true
-- stdpath("data") is ~/.local/share/nvim — neovim manages this directory itself,
-- no need to create it manually. The // suffix encodes the full file path in the
-- undo filename so two files named foo.py in different dirs don't collide.
opt.undodir   = vim.fn.stdpath("data") .. "/undodir//"
opt.swapfile  = false
opt.backup    = false
opt.writebackup = false

-- Ensure undodir exists
vim.fn.mkdir(vim.fn.stdpath("data") .. "/undodir", "p")

-- Performance
opt.lazyredraw = true

-- Visual feedback
opt.showcmd    = true
opt.showmode   = true
opt.ruler      = true
opt.laststatus = 2
opt.wildmenu   = true
opt.wildmode   = "longest:full,full"

-- Splits open naturally
opt.splitbelow = true
opt.splitright = true

-- ============================================================
-- Mappings
-- ============================================================

-- Clear search highlight with Escape
vim.keymap.set("n", "<Esc>", ":nohlsearch<CR>", { silent = true })

-- Telescope (replaces fzf.vim)
-- :Telescope find_files  →  like :Files in fzf.vim
-- :Telescope buffers     →  like :Buffers
-- :Telescope live_grep   →  like :Rg (needs ripgrep on PATH)
local tb = require("telescope.builtin")
vim.keymap.set("n", "<C-p>", tb.find_files, { desc = "Telescope: find files" })
vim.keymap.set("n", "<C-b>", tb.buffers,    { desc = "Telescope: list buffers" })
vim.keymap.set("n", "<C-f>", tb.live_grep,  { desc = "Telescope: live grep" })

-- ============================================================
-- Completion (nvim-cmp)
-- ============================================================
local cmp     = require("cmp")
local luasnip = require("luasnip")

cmp.setup({
  snippet = {
    expand = function(args) luasnip.lsp_expand(args.body) end,
  },
  mapping = cmp.mapping.preset.insert({
    ["<Tab>"]   = cmp.mapping(function(fallback)
                    if cmp.visible() then cmp.select_next_item()
                    elseif luasnip.expand_or_jumpable() then luasnip.expand_or_jump()
                    else fallback() end
                  end, { "i", "s" }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
                    if cmp.visible() then cmp.select_prev_item()
                    elseif luasnip.jumpable(-1) then luasnip.jump(-1)
                    else fallback() end
                  end, { "i", "s" }),
    ["<CR>"]    = cmp.mapping.confirm({ select = false }),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"]   = cmp.mapping.abort(),
  }),
  sources = cmp.config.sources({
    { name = "nvim_lsp" },
    { name = "luasnip" },
    { name = "buffer" },
    { name = "path" },
  }),
})

-- ============================================================
-- LSP (nvim 0.11+ built-in, no lspconfig plugin needed)
-- Language servers must be installed separately:
--   Python:  npm install -g pyright
--   Haskell: ghcup install hls
--   C++:     ships with llvm (check: which clangd)
-- ============================================================

local caps = require("cmp_nvim_lsp").default_capabilities()

vim.lsp.config("pyright", { capabilities = caps })
vim.lsp.config("hls",     { capabilities = caps })
vim.lsp.config("clangd",  { capabilities = caps })

vim.lsp.enable({ "pyright", "hls", "clangd" })

-- LSP keymaps — set when an LSP attaches to a buffer
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(ev)
    local opts = { buffer = ev.buf, silent = true }
    vim.keymap.set("n", "gd", vim.lsp.buf.definition,      opts)
    vim.keymap.set("n", "gr", vim.lsp.buf.references,      opts)
    vim.keymap.set("n", "gy", vim.lsp.buf.type_definition, opts)
    vim.keymap.set("n", "gi", vim.lsp.buf.implementation,  opts)
    vim.keymap.set("n", "K",  vim.lsp.buf.hover,           opts)
    vim.keymap.set("n", "[d", vim.diagnostic.goto_prev,    opts)
    vim.keymap.set("n", "]d", vim.diagnostic.goto_next,    opts)
  end,
})

