
use crate::base::{ListError, ListResult};

/*
 * Implementation of lists of strings as json strings.
 */

pub fn mk_empty() -> ListResult<String> {
    let list: Vec<String> = vec![];
    serde_json::to_string(&list)
        .map_err(|er| ListError::ListJsonConversionError(er.to_string()))
}

fn vec_check_len<T>(vec: Vec<T>, required_index: usize) -> ListResult<Vec<T>>
  where T: ToString {
    if vec.len() >= required_index {
        Ok(vec)
    }
    else {
        // TODO. Hack. Overloading this error for initial expedience.
        Err(ListError::ListIndexOutOfRangeError(vec_to_string(&vec), required_index))
    }
}

fn safe_vec_from_str(json_list: &str, required_index: usize) -> ListResult<Vec<String>> {
    serde_json::from_str(json_list.clone())
        .map_err(|err| ListError::ListJsonConversionError(err.to_string()))
        .and_then(|vec| vec_check_len(vec, required_index))
}

pub fn add_element(json_list: &str, element: &str, index: usize) -> ListResult<String> {
    safe_vec_from_str(json_list, index)
        .and_then( |mut vec| {
            vec.insert(index, element.to_string());
            serde_json::to_string(&vec)
                .map_err(|err| ListError::ListJsonConversionError(err.to_string()))
            })
}

pub fn get_element(json_list: &str, index: usize) -> ListResult<String> {
    safe_vec_from_str(json_list, index)
        .map(|mut vec| vec.remove(index))
}

pub fn vec_to_string<T>(vec: &Vec<T>) -> String
    where T: ToString {
    vec.iter().fold(
        String::new(),
         |acc, element| acc + &element.to_string() + ", ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_element() {
        let tea_str = "tea";
        let coffee_str = "coffee";

        let empty_json = mk_empty().unwrap();
        println!("empty_json {:?}", empty_json);
        let empty_vec: Vec<String> = serde_json::from_str(&empty_json).unwrap();
        println!("empty vector '{}'", vec_to_string(&empty_vec));
        assert_eq!(empty_vec, vec![] as Vec<String>);

        let tea_json = add_element(&empty_json, tea_str, 0).unwrap();
        println!("tea_json {}", tea_json);
        let tea_coffee_json = add_element(&tea_json, coffee_str, 1).unwrap();
        println!("tea_coffee_json {}", tea_coffee_json);
        let tea = get_element(&tea_coffee_json, 0).unwrap();
        assert_eq!(tea, tea_str.to_string());
        let coffee = get_element(&tea_coffee_json, 1).unwrap();
        assert_eq!(coffee, coffee_str.to_string());

        // TODO. Negative tests.
    }
}


