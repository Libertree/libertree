if echo "\dg" | psql -U postgres | grep -wq libertree; then
  echo "User already exists: libertree"
else
  createuser -S -D -R -U postgres libertree
fi
