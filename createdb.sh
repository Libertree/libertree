createdb -U postgres -O libertree libertree_development
createdb -U postgres -O libertree libertree_test
# Install the uuid-ossp module in each database using a PostgreSQL super user:
echo 'CREATE EXTENSION "uuid-ossp";' | psql -U postgres libertree_development
echo 'CREATE EXTENSION "uuid-ossp";' | psql -U postgres libertree_test
