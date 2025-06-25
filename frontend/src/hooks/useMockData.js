// Mock Data Hook
const useMockData = () => {
  const mockInitialResponse = {
    '123': {
      success: true,
      categories: [
        { value: 'electronics', label: 'Electronics' },
        { value: 'books', label: 'Books' },
        { value: 'home', label: 'Home & Garden' }
      ],
      rankingTypes: [
        { value: 'price_low', label: 'Price: Low to High' },
        { value: 'price_high', label: 'Price: High to Low' },
        { value: 'rating', label: 'Best Rating' },
        { value: 'popularity', label: 'Most Popular' },
        { value: 'newest', label: 'Newest First' }
      ]
    },
    '456': {
      success: true,
      categories: [
        { value: 'clothing', label: 'Clothing' },
        { value: 'sports', label: 'Sports' }
      ],
      rankingTypes: [
        { value: 'price_low', label: 'Price: Low to High' },
        { value: 'price_high', label: 'Price: High to Low' },
        { value: 'rating', label: 'Best Rating' }
      ]
    },
    '999': {
      success: false,
      error: 'Invalid ID or access denied'
    }
  };

  const mockServerResponse = {
    '123-electronics': {
      price_low: [
        { name: 'Budget Phone', price: 199 },
        { name: 'Basic Tablet', price: 299 },
        { name: 'Standard Laptop', price: 599 },
        { name: 'Gaming Mouse', price: 79 },
        { name: 'USB Cable', price: 15 }
      ],
      price_high: [
        { name: 'Premium Laptop', price: 1899 },
        { name: 'High-end Phone', price: 1299 },
        { name: 'Gaming Desktop', price: 2499 },
        { name: 'Smart TV', price: 899 },
        { name: 'Wireless Headphones', price: 349 }
      ],
      rating: [
        { name: 'Top Rated Phone', price: 899 },
        { name: 'Best Laptop', price: 1299 },
        { name: 'Excellent Tablet', price: 499 },
        { name: 'Great Headphones', price: 199 },
        { name: 'Perfect Mouse', price: 89 }
      ],
      popularity: [
        { name: 'Popular Phone', price: 699 },
        { name: 'Trending Laptop', price: 999 },
        { name: 'Hot Tablet', price: 399 },
        { name: 'Viral Headphones', price: 149 },
        { name: 'Bestseller Mouse', price: 59 }
      ],
      newest: [
        { name: 'New Phone 2025', price: 1099 },
        { name: 'Latest Laptop', price: 1599 },
        { name: 'Fresh Tablet', price: 699 },
        { name: 'Modern Headphones', price: 249 },
        { name: 'New Mouse', price: 99 }
      ]
    },
    '123-books': {
      price_low: [
        { name: 'Paperback Novel', price: 8 },
        { name: 'Used Textbook', price: 15 },
        { name: 'Comic Book', price: 4 },
        { name: 'Magazine', price: 6 },
        { name: 'Pocket Dictionary', price: 12 }
      ],
      price_high: [
        { name: 'Medical Textbook', price: 299 },
        { name: 'Art Coffee Table Book', price: 89 },
        { name: 'Collector Edition', price: 149 },
        { name: 'Technical Manual', price: 199 },
        { name: 'Signed First Edition', price: 399 }
      ],
      rating: [
        { name: 'Bestseller Novel', price: 25 },
        { name: '5-Star Cookbook', price: 35 },
        { name: 'Award Winner', price: 18 },
        { name: 'Top Biography', price: 28 },
        { name: 'Acclaimed Poetry', price: 22 }
      ],
      popularity: [
        { name: 'Trending Romance', price: 16 },
        { name: 'Popular Mystery', price: 19 },
        { name: 'Viral Self-Help', price: 24 },
        { name: 'Hit Fantasy', price: 21 },
        { name: 'Famous Memoir', price: 26 }
      ],
      newest: [
        { name: 'New Release 2025', price: 29 },
        { name: 'Fresh Sci-Fi', price: 27 },
        { name: 'Latest Thriller', price: 23 },
        { name: 'New Biography', price: 31 },
        { name: 'Recent History', price: 33 }
      ]
    },
    '456-clothing': {
      price_low: [
        { name: 'Basic T-Shirt', price: 12 },
        { name: 'Cotton Socks', price: 8 },
        { name: 'Simple Jeans', price: 29 },
        { name: 'Plain Cap', price: 15 },
        { name: 'Basic Underwear', price: 10 }
      ],
      price_high: [
        { name: 'Designer Jacket', price: 599 },
        { name: 'Luxury Dress', price: 399 },
        { name: 'Premium Suit', price: 899 },
        { name: 'High-end Shoes', price: 299 },
        { name: 'Cashmere Sweater', price: 249 }
      ],
      rating: [
        { name: 'Top Rated Jeans', price: 89 },
        { name: 'Best Jacket', price: 149 },
        { name: 'Excellent Shoes', price: 99 },
        { name: 'Perfect Shirt', price: 45 },
        { name: 'Great Dress', price: 79 }
      ],
      popularity: [
        { name: 'Trending Hoodie', price: 59 },
        { name: 'Popular Sneakers', price: 79 },
        { name: 'Hot Jeans', price: 69 },
        { name: 'Viral T-Shirt', price: 25 },
        { name: 'Bestseller Jacket', price: 99 }
      ],
      newest: [
        { name: 'New Collection Dress', price: 129 },
        { name: 'Latest Sneakers', price: 119 },
        { name: 'Fresh Style Jacket', price: 159 },
        { name: 'Modern Jeans', price: 89 },
        { name: 'New Season Shirt', price: 49 }
      ]
    }
  };

  const detailsList = [
    { name: 'John Smith', time: '14:32:15' },
    { name: 'Sarah Johnson', time: '14:28:42' },
    { name: 'Mike Davis', time: '14:25:18' },
    { name: 'Emma Wilson', time: '14:21:33' },
    { name: 'Alex Brown', time: '14:18:07' },
    { name: 'Lisa Garcia', time: '14:14:52' },
    { name: 'David Miller', time: '14:11:24' },
    { name: 'Anna Rodriguez', time: '14:08:16' },
    { name: 'Tom Anderson', time: '14:05:39' },
    { name: 'Maria Martinez', time: '14:02:08' },
    { name: 'Chris Taylor', time: '13:59:45' },
    { name: 'Jessica White', time: '13:56:21' },
    { name: 'Kevin Lee', time: '13:53:07' },
    { name: 'Amy Jackson', time: '13:49:33' },
    { name: 'Ryan Thompson', time: '13:46:18' },
    { name: 'Nicole Harris', time: '13:43:02' },
    { name: 'Daniel Clark', time: '13:39:47' },
    { name: 'Rachel Lewis', time: '13:36:29' },
    { name: 'Mark Walker', time: '13:33:14' },
    { name: 'Sofia Hall', time: '13:30:01' }
  ];

  return { mockInitialResponse, mockServerResponse, detailsList };
};

export default useMockData;
