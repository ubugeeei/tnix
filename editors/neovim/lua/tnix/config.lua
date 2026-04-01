local M = {}

local default_cmd = { "tnix-lsp" }
local default_root_markers = { "flake.nix", "cabal.project", "pnpm-workspace.yaml", ".git" }

local function clone(list)
  return vim.deepcopy(list)
end

local function normalize_list(values)
  local normalized = {}

  if type(values) ~= "table" then
    return normalized
  end

  for _, value in ipairs(values) do
    if type(value) == "string" then
      local trimmed = vim.trim(value)
      if trimmed ~= "" then
        table.insert(normalized, trimmed)
      end
    end
  end

  return normalized
end

---Normalize the configured tnix-lsp command into argv form.
---
---The helper accepts either a string or a list so users can configure the
---language server in the style that is most natural for their Neovim setup.
---@param cmd string|string[]|nil
---@return string[]
function M.normalize_cmd(cmd)
  if type(cmd) == "string" then
    local trimmed = vim.trim(cmd)
    if trimmed ~= "" then
      return { trimmed }
    end
  end

  local normalized = normalize_list(cmd)
  if #normalized > 0 then
    return normalized
  end

  return clone(default_cmd)
end

---Resolve the project-root markers used for workspace discovery.
---@param markers string[]|nil
---@return string[]
function M.root_markers(markers)
  local normalized = normalize_list(markers)
  if #normalized > 0 then
    return normalized
  end

  return clone(default_root_markers)
end

---Resolve the workspace root for the current buffer or path.
---
---An explicit string or callback wins over marker-based discovery so callers
---can integrate tnix into unusual repository layouts when needed.
---@param source integer|string
---@param opts table|nil
---@return string
function M.resolve_root_dir(source, opts)
  opts = opts or {}

  if type(opts.root_dir) == "function" then
    local resolved = opts.root_dir(source)
    if type(resolved) == "string" and resolved ~= "" then
      return resolved
    end
  end

  if type(opts.root_dir) == "string" and opts.root_dir ~= "" then
    return opts.root_dir
  end

  return vim.fs.root(source, M.root_markers(opts.root_markers)) or vim.loop.cwd()
end

---Build the `vim.lsp.start` config used by the tnix plugin.
---@param source integer|string
---@param opts table|nil
---@return table
function M.server_config(source, opts)
  opts = opts or {}

  return {
    name = opts.name or "tnix-lsp",
    cmd = M.normalize_cmd(opts.cmd),
    cmd_env = type(opts.cmd_env) == "table" and opts.cmd_env or nil,
    root_dir = M.resolve_root_dir(source, opts),
  }
end

return M
