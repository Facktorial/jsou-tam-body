import DetailsList from './DetailsList';
import PointsPredictor from './PointsPredictor';

const ServerResponse = ({ serverData, availableOptions, category,
    getCurrentRankingData, showDetails, setShowDetails, detailsList,
    getCoefficient, getTopRunners, getRunners, enabledPredictor, setEnablingPredictor
}) => {
  if (!serverData) return null;

  const coefficient = getCoefficient ? getCoefficient() : null;
  const topRunners = getTopRunners ? getTopRunners() : [];

  return (
    <div>
    <div className="bg-gray-900 p-4 rounded border border-yellow-600">
      <div className="text-yellow-400 font-mono">
        <span className="font-semibold mr-2">Base Coefficient:</span>
        <span className="font-bold text-lg">{getCoefficient() || 'N/A'}</span>
      </div>
      {serverData.error ? (
        <p className="text-red-400 font-mono">{serverData.error}</p>
      ) : (
        <div className="mt-6">
          <div>
            <h4 className="text-yellow-400 font-mono mb-2">Top Runners:</h4>
            <div className="mb-6">
              {getTopRunners().map((runner, index) => (
                <div key={index} className="flex justify-between items-center text-sm">
                  <span className="text-yellow-200 font-mono">
                    {index + 1}. {runner.racerName}
                  </span>
                  <div className="flex gap-2">
                    <span className="text-yellow-400 font-mono">
                      {runner.racerCoef}
                    </span>
                    <span className="text-yellow-500">
                      (#{runner.racerRanking})
                    </span>
                  </div>
                </div>
              ))}
              {getTopRunners().length === 0 && (
                <div className="text-yellow-600 text-xs">No runners with valid coefficients</div>
              )}
            </div>
          </div>
          
          <button
            onClick={() => setShowDetails(!showDetails)}
            className="mt-3 px-4 py-2 bg-gray-800 text-yellow-200 border border-yellow-600 rounded font-mono hover:bg-yellow-600 hover:text-black focus:bg-yellow-500 focus:text-black transition-colors"
          >
            {showDetails ? 'Hide Plebs' : 'Show All'}
          </button>

          <DetailsList showDetails={showDetails} detailsList={detailsList} getRunners={getRunners}/>
        </div>
      )}
    </div>
      { enabledPredictor ? ( 
      <PointsPredictor
        serverData={serverData}
        BP={getCoefficient()}
        category={category}
        rankingType={1}
      />
      ) : (<div></div>)}
  </div>
  );
};

export default ServerResponse;
