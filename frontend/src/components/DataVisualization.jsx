import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, Cell } from 'recharts';

const DataVisualization = ({
  getCurrentRankingData, availableOptions, category, rankingType, setRankingType, serverData, rankingTypeNames
}) => {
  const currentData = getCurrentRankingData();

  //if (!currentData) return null;
  const getCategoryData = () => {
    if (!serverData || serverData.error || !category) return null;

    // Create visualization data from all available ranking types
    const chartData = availableOptions?.rankingTypes?.map(currentRankingType => {
      const data = serverData.eventResult.coefs[currentRankingType.value];
      const value = data[category];
  
      console.log("CATEGORY", category);
      console.log("DATA CHART", data);
      const categoryValues = Object.values(data)
      console.log("value", rankingType);
      console.log("current", currentRankingType.value);
      console.log("================================");

      return {
        name: currentRankingType.label,
        value: value,
        rankingTypeValue: currentRankingType.value,
        count: 1,
        total: 1.00,
        isSelected: rankingType == currentRankingType.value,
      };
    }).filter(Boolean);
    
    return chartData;
  };

  const chartData = getCategoryData();
  
  if (!chartData || chartData.length === 0) return null;

  const handleBarClick = (data) => {
    if (data && data.rankingTypeValue !== undefined && setRankingType) {
      setRankingType(data.rankingTypeValue);
    }
  };

  return (
    <div>
      <h4 className="text-yellow-400 font-mono mb-2">
        Rankings overview: {availableOptions?.category?.find(r => r.value === category)?.label}
      </h4>
      
      <div className="bg-black p-3 rounded border border-yellow-700">
        <ResponsiveContainer width="100%" height={200}>
          <BarChart data={chartData}>
            <CartesianGrid strokeDasharray="3 3" stroke="#525252" />
            <XAxis 
              dataKey="name" 
              tick={{ fontSize: 10, fill: '#FDE047' }}
              angle={-45}
              textAnchor="end"
              height={60}
            />
            <YAxis
              tick={{ fontSize: 10, fill: '#FDE047' }}
              domain={['dataMin - 50', 'dataMax + 50']}
            />
            <Tooltip 
              contentStyle={{ 
                backgroundColor: '#111827', 
                border: '1px solid #CA8A04',
                color: '#FDE047',
                fontFamily: 'monospace'
              }}
              labelFormatter={(label) => {
                return `${label}`;
              }}
              content={({ active, payload, label }) => {
                if (active && payload && payload.length) {
                  const data = payload[0];
                  return (
                    <div style={{
                      backgroundColor: '#111827',
                      border: '1px solid #CA8A04',
                      color: '#FDE047',
                      fontFamily: 'monospace',
                      padding: '8px',
                      borderRadius: '4px'
                    }}>
                      <p>{`${label}`}</p>
                      <p>{`Coefficient: ${data.value !== undefined ? data.value : 'No data'}`}</p>
                    </div>
                  );
                }
                return null;
              }}
            />
            <Bar
              dataKey="value"
              onClick={handleBarClick}
              style={{ cursor: 'pointer' }}
            >
              {chartData.map((entry, index) => (
                <Cell
                  key={`cell-${index}`}
                  fill={entry.isSelected ? '#D2042D' : '#CA8A04'}
                />
              ))}
            </Bar>
          </BarChart>
        </ResponsiveContainer>
      </div>
    </div>
  );
};

export default DataVisualization;
