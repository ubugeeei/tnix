local M = {}
local config = require("tnix.config")

function M.setup(opts)
  opts = opts or {}

  vim.filetype.add({
    extension = { tnix = "tnix" },
    pattern = { [".*%.d%.tnix"] = "tnix" },
  })

  vim.api.nvim_create_autocmd("FileType", {
    pattern = "tnix",
    callback = function(ev)
      vim.lsp.start(config.server_config(ev.buf, opts))
    end,
  })
end

return M
