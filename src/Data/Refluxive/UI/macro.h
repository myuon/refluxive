#define MAKE_LENS(label,type,new_name,name) new_name :: Lens' (Model label) type; new_name = lens name (\s a -> s { name = a })

