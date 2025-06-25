import React, { useState, useEffect } from 'react';
import ServerConfig from './components/ServerConfig';
import FormControls from './components/FormControl';
import ServerResponse from './components/ServerResponse';
import useMockData from './hooks/useMockData';


export default function KolikTamJeBoduView() {
  const [id, setId] = useState('');
  const [hOrDCategory, setHOrDCategory] = useState('');
  const [category, setCategory] = useState('');
  const [rankingType, setRankingType] = useState('');
  const [rankingTypeNames, setRankingTypeNames] = useState('');
  const [serverData, setServerData] = useState(null);
  const [availableOptions, setAvailableOptions] = useState(null);
  const [isInitialLoading, setIsInitialLoading] = useState(false);
  const [showDetails, setShowDetails] = useState(false);
  const [requestStatus, setRequestStatus] = useState('idle');
  const [serverUrl, setServerUrl] = useState('http://localhost:8000/api');
  const [showServerConfig, setShowServerConfig] = useState(false);
  const [forcingAge, setForcingAge] = useState(true);
  const [enabledPredictor, setEnablingPredictor] = useState(true);

  const { mockInitialResponse, mockServerResponse, detailsList } = useMockData();

  const getCurrentRankingData = () => {
    if (!serverData || serverData.error || !category) return null;
    return serverData[category] || null;
  };

  // Helper function to extract categories from server response
  const extractCategoriesFromResponse = (data) => {
    if (!data?.eventResult?.coefs?.[0]) return [];
    
    const categories = Object.keys(data.eventResult.coefs[0]);
    return categories.map(cat => ({
      value: cat,
      label: cat
    }));
  };

    // Helper function to extract ranking types from server response
  const extractRankingTypesFromResponse = (data) => {
    if (!data?.rankingtypes?.Data) return [];
    
    const rankingTypes = Object.values(data.rankingtypes.Data).map(rankType => ({
      value: rankType.ID - 1,
      label: rankType.NameEN
    }));
    
    return rankingTypes;
  };

  // Helper function to extract list of NameEN ranking types
  const extractRankingTypeNames = (data) => {
    if (!data?.rankingtypes?.Data) return [];
    
    return Object.values(data.rankingtypes.Data).map(rankType => rankType.NameEN);
  };

    // Function to get coefficient for selected category and ranking
  const getCoefficient = () => {
    if (!serverData?.eventResult?.coefs || !category || rankingType === '') return null;
    return serverData.eventResult.coefs[rankingType]?.[category] || null;
  };

  const getRunners = () => {
    if (!serverData?.eventResult?.racers || !category || rankingType === '') return [];
    const runners = serverData.eventResult.racers[rankingType]?.[category] || [];
    return runners;
  };

  const getTopRunners = () => {
    return getRunners().slice(0, 4).filter(runner => runner.racerCoef >= 0); // Filter out 0 coef runners
  };

  const handleInitialRequest = async () => {
    if (!id.trim() || !hOrDCategory) return;
    
    setIsInitialLoading(true);
    setRequestStatus('idle');
    setAvailableOptions(null);
    setCategory('');
    setRankingType('');
    setServerData(null);
    
    const requestUrl = `http://localhost:8000/api/${id}/${hOrDCategory}/${forcingAge}`;
    console.log('Sending initial request to:', requestUrl);
    console.log('Request parameters:', { raceId: id, category: hOrDCategory, forceAge: forcingAge });
    
    try {
      const response = await fetch(requestUrl, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
        },
      });
      
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      
      const data = await response.json();
      console.log('Server response:', data);

      if (data) { console.log('success??', data.message); }
      
      if (data && data.success) {
        // Extract categories and ranking types from the response
        const categories = extractCategoriesFromResponse(data);
        const rankingTypes = extractRankingTypesFromResponse(data);
        
        setRankingTypeNames(extractRankingTypeNames(data));

        const updatedOptions = {
          success: true,
          categories: categories,
          rankingTypes: rankingTypes,
          rankingTypeNames: rankingTypeNames
        };
        setAvailableOptions(updatedOptions);

        console.log('Extracted categories:', categories);
        console.log('Extracted ranking types:', rankingTypes);
        console.log('Ranking type names:', rankingTypeNames);

        setServerData(data);
        setRequestStatus('success');

        if (categories.length > 0) {
          setCategory(categories[0].value);
        }
        if (rankingTypes.length > 0) {
          setRankingType(rankingTypes[0].value);
        }

        console.log('Server response - available options:', data);
      } else {
        setRequestStatus('error');
        console.log('Server error:', data?.error || 'Unknown error');
      }
      
    } catch (error) {
      console.error('Request failed:', error);
      setRequestStatus('error');
      
      // Optionally, you can set an error message for the user
      setAvailableOptions({
        error: `Failed to connect to server: ${error.message}`
      });
    } finally {
      setIsInitialLoading(false);
    }
  };

  // useEffect(() => {
  //   if (requestStatus === 'success' && category) {
  //     const requestKey = `${id}-${category}`;
  //     console.log('Auto-loading data for:', { id, hOrDCategory, category });
  //     
  //     const response = mockServerResponse[requestKey];
  //     if (response) {
  //       setServerData(response);
  //       console.log('Data loaded:', response);
  //     } else {
  //       setServerData({ error: 'No data found for this ID and category combination' });
  //     }
  //     setShowDetails(false);
  //   } else if (!category) {
  //     setServerData(null);
  //   }
  // }, [category, requestStatus, id, hOrDCategory, mockServerResponse]);

  return (
    <div 
      className="min-h-screen p-8"
      style={{
        background: 'linear-gradient(135deg, #f5f1e8 0%, #ede4d3 50%, #e8ddc7 100%)',
        backgroundImage: `
          radial-gradient(circle at 25% 25%, rgba(255,255,255,0.2) 0%, transparent 50%),
          radial-gradient(circle at 75% 75%, rgba(0,0,0,0.05) 0%, transparent 50%),
          linear-gradient(45deg, rgba(255,255,255,0.1) 25%, transparent 25%),
          linear-gradient(-45deg, rgba(255,255,255,0.1) 25%, transparent 25%)
        `,
        backgroundSize: '200px 200px, 200px 200px, 40px 40px, 40px 40px'
      }}
    >
      <div className="max-w-md mx-auto bg-black rounded-lg shadow-2xl border border-yellow-500 backdrop-blur-sm">
        <div className="p-6">
          <div className="flex justify-between items-center mb-4">
            <h2 className="text-2xl font-bold text-yellow-400">Jsou tam body?</h2>
            <button
              onClick={() => setShowServerConfig(!showServerConfig)}
              className="text-xs px-2 py-1 bg-gray-800 text-yellow-200 border border-yellow-600 rounded font-mono hover:bg-yellow-600 hover:text-black"
            >
              Config
            </button>
          </div>

          <ServerConfig
            serverUrl={serverUrl}
            setServerUrl={setServerUrl}
            showConfig={showServerConfig}
            setShowConfig={setShowServerConfig}
            forcingAge={forcingAge}
            setForcingAge={setForcingAge}
            enabledPredictor={enabledPredictor}
            setEnablingPredictor={setEnablingPredictor}
          />
          
          <FormControls
            id={id}
            setId={setId}
            hOrDCategory={hOrDCategory}
            setHOrDCategory={setHOrDCategory}
            category={category}
            setCategory={setCategory}
            rankingType={rankingType}
            setRankingType={setRankingType}
            rankingTypeNames={rankingTypeNames}
            requestStatus={requestStatus}
            availableOptions={availableOptions}
            handleInitialRequest={handleInitialRequest}
            isInitialLoading={isInitialLoading}
            mockInitialResponse={mockInitialResponse}
            getCurrentRankingData={getCurrentRankingData}
            serverData={serverData}
          />

          <ServerResponse
            serverData={serverData}
            availableOptions={availableOptions}
            rankingType={rankingType}
            category={category}
            showDetails={showDetails}
            setShowDetails={setShowDetails}
            detailsList={detailsList}
            getCoefficient={getCoefficient}
            getRunners={getRunners}
            getTopRunners={getTopRunners}
            enabledPredictor={enabledPredictor}
            setEnablingPredictor={setEnablingPredictor}
          />
        </div>
      </div>
    </div>
  );
}
