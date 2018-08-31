# TODO: Write documentation for `Permutations`

def perms_for_str_aux (word, new_word, used)
  (puts "#{new_word}" ; return) if new_word.size == word.size

  word.each_char_with_index do |ch, i|
    if !used[i]
      used[i] = true
      perms_for_str_aux word, new_word + ch, used
      used[i] = false
    end
  end
end


def perms_for_str (word)
  perms_for_str_aux word, "", Array.new word.size, false
end

perms_for_str "abc"
