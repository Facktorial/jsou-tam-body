// Configuration Component
const ServerConfig = ({ serverUrl, setServerUrl, showConfig, setShowConfig, forcingAge, setForcingAge, enabledPredictor, setEnablingPredictor }) => {
  if (!showConfig) return null;
  
  const handleCheckboxChange = (event) => {
    setForcingAge(event.target.checked);
  };

  const handleEnablingPredictor = (event) => {
    console.log(event);
    setEnablingPredictor(event.target.checked);
    console.log(event);
  };

  return (
    <div className="mb-4 p-3 bg-gray-900 border border-yellow-600 rounded">
      <h3 className="text-yellow-400 font-mono text-sm mb-2">Server Configuration</h3>
      <input
        type="text"
        value={serverUrl}
        onChange={(e) => setServerUrl(e.target.value)}
        placeholder="Server URL"
        className="w-full p-2 bg-gray-800 border border-yellow-600 rounded text-yellow-200 placeholder-yellow-700 focus:outline-none focus:ring-2 focus:ring-yellow-500 font-mono text-sm"
      />
      <p className="text-yellow-600 text-xs mt-1 font-mono mb-2">
        Endpoint: POST /check (id, category) → returns data + options
      </p>

      <div className="text-yellow-400 font-mono text-base flex items-center justify-between w-full">
        Force age:
        <label className="text-yellow-400">
          <input
            className="accent-yellow-500 w-4 h-4"
            type="checkbox"
            checked={forcingAge}
            onChange={handleCheckboxChange}
          />
        </label>
      </div>

      <div className="text-yellow-400 font-mono text-base flex items-center justify-between w-full">
        Predictor enabled:
        <label className="text-yellow-400">
          <input
            className="accent-yellow-500 w-4 h-4"
            type="checkbox"
            checked={enabledPredictor}
            onChange={handleEnablingPredictor}
          />
        </label>
      </div>
    </div>
  );
};

export default ServerConfig;
