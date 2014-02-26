/*
 * Spring 2014 CSCI182E - Distributed Systems
 * Harvey Mudd College
 * Assignment 3 - RMI and Erlang Clients/Servers
 * Written by Daniel M. Zimmerman
 */

package words;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeSet;

/**
 * WordList encapsulates a word list and distance calculation for 
 * suggested spellings.
 *
 * @author Daniel M. Zimmerman
 * @version Spring 2014
 */
public class WordList {
  // Instance Fields
  
  /**
   * The TreeSet containing the word list. A TreeSet was chosen because it
   * sorts the entries and allows for relatively quick searching.
   */
  private final Set<String> my_words = new TreeSet<String>();
  
  // Constructor
  
  /**
   * Creates a WordList from the file with the specified name.
   *
   * @param the_file The name of the file.
   * @exception IOException if there is a problem reading the list from the 
   *  file.
   */
  public WordList(final String the_file) {    
    try {
      // create a reader for the file
      final FileReader file_reader = new FileReader(the_file);
      final BufferedReader buffered_reader = new BufferedReader(file_reader);
    
      // read the lines
    
      String line;
    
      do {
        line = buffered_reader.readLine();
      
        if (line != null && line.length() != 0) 
        {
          // it's a word (not a blank line)
          
          my_words.add(line);
        }
      } while (line != null);
      
      buffered_reader.close();
    } catch (final IOException e) {
      my_words.clear();
    }
  }
  
  // Static Methods
  
  /**
   * Computes the distance between two strings, up to a specified threshold. 
   * 
   * @param word_1 The first word.
   * @param word_2 The second word.
   * @param the_threshold The threshold.
   * @return The distance between the two strings, or an arbitrary number greater 
   * than the_threshold if the distance is greater than the_threshold.
   */
  public static int distance(final String word_1, final String word_2, 
                             final int the_threshold) {
    return levenshtein(word_1, word_2, the_threshold, 0);
  }
  
  /**
   * Computes the Levenshtein distance between two strings, recursively,
   * up to a specified threshold. 
   * 
   * @param word_1 The first word.
   * @param word_2 The second word.
   * @param pos_1 The position in the first word.
   * @param pos_2 The position in the second word.
   * @param the_threshold The threshold.
   * @param the_distance The distance counted so far.
   * @return the Levenshtein distance.
   */
  private static int levenshtein(final String word_1, final String word_2,
                                 final int the_threshold, final int the_distance) {
    int result = 0;
    if (the_distance > the_threshold) {
      return the_distance;
    } else if (word_1.length() == 0) {
      result = word_2.length();
    } else if (word_2.length() == 0) {
      result = word_1.length();
    } else {
      final int l1 = levenshtein(word_1.substring(1), word_2, 
                                 the_threshold, the_distance + 1) + 1;
      final int l2 = levenshtein(word_1, word_2.substring(1), 
                                 the_threshold, the_distance + 1) + 1;
      int delta = 0;
      if (word_1.charAt(0) != word_2.charAt(0)) {
        delta = 1;
      }      
      int l3 = levenshtein(word_1.substring(1), word_2.substring(1),
                           the_threshold, the_distance + delta) + delta;
      result = Math.min(l1, l2);
      result = Math.min(result, l3);
    }
    return result;
  }
  
  // Instance Methods
  
  /**
   * Checks for a word in the list.
   * 
   * @param the_word The word to search for.
   * @return true if the specified word is in the word list, false 
   *  otherwise.
   */
  public synchronized boolean correct(final String the_word) {
    return my_words.contains(the_word);
  }

  /**
   * Checks for suggested spellings for words in the list.
   * 
   * @param the_word The word to check for suggested spellings.
   * @return a list containing all the suggested spellings for 
   * the specified word, as described in the assignment 
   */
  public synchronized List<String> suggestions(final String the_word) {
    final List<String> suggestions_list = new ArrayList<String>();
    
    for (String s : my_words) {
      if (distance(the_word, s, 1) <= 1) {
        // the word is close enough
        suggestions_list.add(s);
      }
    }
    
    return suggestions_list;
  }
  
  /**
   * Adds the specified word to the word list. If the word is
   * already in the list, this method has no effect.
   * 
   * @param the_word The word to add to the word list.
   * @return true if the word was added, false otherwise.
   */
  public synchronized boolean add(final String the_word) {
    return my_words.add(the_word);
  }
  
  /**
   * Removes the specified word from the word list. If the word is
   * already in the list, this method has no effect.
   * 
   * @param the_word The word to remove from the word list.
   * @return true if the word was added, false otherwise.
   */
  public synchronized boolean remove(final String the_word) {
    return my_words.remove(the_word);
  }
  
  /**
   * The main method, continually reads words and checks their spellings/
   * suggestions.
   * 
   * @param the_args Command line arguments.
   */
  public static void main(final String[] the_args) {
    final WordList wl = new WordList(the_args[0]);
    final Scanner scanner = new Scanner(System.in);
    while (scanner.hasNextLine()) {
      final String word = scanner.nextLine();
      System.out.println(wl.correct(word));
      System.out.println(wl.suggestions(word));
    }
    scanner.close();
  }
}
