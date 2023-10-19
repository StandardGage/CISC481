
(defvar node-1 (create_node "baggor" 20 0 nil 0 nil 'get_estimate_cost_to_goal_PE "durant"))

(defvar node-2 (create_node "crent" 7 800 node-1 800 '("baggor" "crent" ROCKET) 'get_estimate_cost_to_goal_PE "durant"))

(defvar node-3 (create_node "durant" 7 1115 node-2 1915 '("crent" "durant" TRANSPORTER) 'get_estimate_cost_to_goal_PE "durant"))
