for db in "test" "development" "production"; do
  if psql -U postgres -l | grep -wq "libertree_$db"; then
    echo "Already exists: libertree_$db"
  else
    createdb -U postgres -O libertree -E UTF8 "libertree_$db"
  fi
done
