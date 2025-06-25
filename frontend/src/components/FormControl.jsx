import StatusMessages from './StatusMessage';
import DataVisualization from './DataVisualization';

const FormControls = ({ 
  id, setId, hOrDCategory, setHOrDCategory, category, setCategory, forcingAge,
  rankingType, setRankingType, requestStatus, availableOptions,
  handleInitialRequest, isInitialLoading, rankingTypeNames,

  mockInitialResponse, getCurrentRankingData, serverData
}) => {
  const checkingText = "Checking...";
  const checkingButtonText = "Body?";

  const getCurrentRankingDataForVisualization = () => {
    if (!serverData?.eventResult?.racers || !category) return null;
    
    // If ranking type is selected, use it as index, otherwise use "0" as default
    const rankingIndex = rankingType || "0";
    return serverData.eventResult.racers[rankingIndex]?.[category] || null;
  };

  return (
    <div className="mb-4 space-y-3">
      <div className="flex gap-2">
        <input
          type="text"
          value={id}
          onChange={(e) => setId(e.target.value)}
          placeholder="Enter Race ID (try 123, 456, or 999)"
          className="flex-1 min-w-0 sm:flex-1 md:w-48 lg:w-64 p-2 bg-gray-900 border border-yellow-600 rounded text-yellow-200 placeholder-yellow-700 focus:outline-none focus:ring-2 focus:ring-yellow-500 font-mono"
        />
        <select
          value={hOrDCategory}
          onChange={(e) => setHOrDCategory(e.target.value)}
          className="p-2 bg-gray-900 border border-yellow-600 rounded text-yellow-200 focus:outline-none focus:ring-2 focus:ring-yellow-500 font-mono"
        >
          <option value="" className="bg-gray-900 text-yellow-700">H/D</option>
          <option value="H" className="bg-gray-900 text-yellow-200">H</option>
          <option value="D" className="bg-gray-900 text-yellow-200">D</option>
        </select>
        <button
          onClick={() => handleInitialRequest(id, hOrDCategory, forcingAge)}
          disabled={!id.trim() || !hOrDCategory || isInitialLoading}
          className="px-4 py-2 bg-yellow-600 text-black rounded hover:bg-yellow-500 disabled:bg-gray-700 disabled:text-gray-400 font-mono font-bold"
        >
          {isInitialLoading ? checkingText : checkingButtonText}
        </button>
      </div>
      
      {requestStatus === 'success' && (
        <div className="text-xs text-yellow-600 mt-2">
          <details>
            <summary className="cursor-pointer">{serverData?.eventInfo.eventName}</summary>
            <ul className="mt-1 ml-4">
                <li className="text-yellow-500">• {serverData?.eventInfo.eventDiscipline}</li>
                <li className="text-yellow-500">• {serverData?.eventInfo.eventPlace}</li>
            </ul>
          </details>
        </div>
      )}

      <select
        value={category}
        onChange={(e) => setCategory(e.target.value)}
        disabled={requestStatus !== 'success'}
        className={`w-full p-2 rounded font-mono focus:outline-none focus:ring-2 ${
          requestStatus !== 'success' 
            ? 'bg-gray-700 border border-gray-500 text-gray-400 cursor-not-allowed' 
            : 'bg-gray-900 border border-yellow-600 text-yellow-200 focus:ring-yellow-500 hover:border-yellow-500'
        }`}
      >
        <option value="" className={requestStatus !== 'success' ? 'bg-gray-700 text-gray-400' : 'bg-gray-900 text-yellow-700'}>
          {requestStatus !== 'success' ? 'Check Race ID and H/D first' : 'Select Category'}
        </option>
        {availableOptions?.categories?.map(cat => (
          <option key={cat.value} value={cat.value} className="bg-gray-900 text-yellow-200">
            {cat.label}
          </option>
        ))}
      </select>

      {category && (
        <DataVisualization
          getCurrentRankingData={getCurrentRankingDataForVisualization}
          availableOptions={availableOptions}
          category={category}
          rankingType={rankingType}
          setRankingType={setRankingType}
          rankingTypeNames={rankingTypeNames}
          serverData={serverData}
        />
      )}

      <select
        value={rankingType}
        onChange={(e) => setRankingType(e.target.value)}
        disabled={requestStatus !== 'success'}
        className={`w-full p-2 rounded font-mono focus:outline-none focus:ring-2 ${
          requestStatus !== 'success' 
            ? 'bg-gray-700 border border-gray-500 text-gray-400 cursor-not-allowed' 
            : 'bg-gray-900 border border-yellow-600 text-yellow-200 focus:ring-yellow-500 hover:border-yellow-500'
        }`}
      >
        <option value="" className={requestStatus !== 'success' ? 'bg-gray-700 text-gray-400' : 'bg-gray-900 text-yellow-700'}>
          {requestStatus !== 'success' ? 'Check Race ID and H/D first' : 'Select Ranking Type'}
        </option>
        {availableOptions?.rankingTypes?.map(rank => (
          <option key={rank.value} value={rank.value} className="bg-gray-900 text-yellow-200">
            {rank.label}
          </option>
        ))}
      </select>
    </div>
  );
};

export default FormControls;
      // <StatusMessages
      //   requestStatus={requestStatus}
      //   id={id}
      //   mockInitialResponse={mockInitialResponse}
      //   availableOptions={availableOptions}
      //   serverData={serverData}
      // />

      // {/* Debug information - remove in production */}
      // {availableOptions?.rankingTypeNames && (
      //   <div className="text-xs text-yellow-600 mt-2">
      //     <details>
      //       <summary className="cursor-pointer">Available Ranking Types (Debug)</summary>
      //       <ul className="mt-1 ml-4">
      //         {availableOptions.rankingTypeNames.map((name, index) => (
      //           <li key={index} className="text-yellow-500">• {name}</li>
      //         ))}
      //       </ul>
      //     </details>
      //   </div>
      // )}
