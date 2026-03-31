local M = {}

function M.setup(opts)
  opts = opts or {}

  vim.filetype.add({
    extension = { tnix = "tnix" },
    pattern = { [".*%.d%.tnix"] = "tnix" },
  })

  vim.api.nvim_create_autocmd("FileType", {
    pattern = "tnix",
    callback = function(ev)
      vim.lsp.start({
        name = "tnix-lsp",
        cmd = opts.cmd or { "tnix-lsp" },
        root_dir = vim.fs.root(ev.buf, { "flake.nix", "cabal.project", ".git" }) or vim.loop.cwd(),
      })
    end,
  })
end

return M
