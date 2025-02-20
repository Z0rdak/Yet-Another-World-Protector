package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.core.INbtSerializable;
import net.minecraft.nbt.CompoundTag;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Represents a container holding flag values mapped to their corresponding flag names for a region.
 * Provides methods for adding, retrieving, updating, and removing flags, as well as handling their states.
 * <br><br>
 * Flags are stored as key-value pairs:
 * <ul>
 *   <li><b>Key:</b> Flag name as a {@code String}</li>
 *   <li><b>Value:</b> Corresponding {@link IFlag} instance</li>
 * </ul>
 * Example:
 * <pre>
 * "break_blocks" -> BooleanFlag {"state": "Denied", ...}
 * </pre>
 * This interface allows for checking flag existence, retrieving active flags, and filtering by {@link FlagState}.
 */
public interface IFlagContainer extends INbtSerializable<CompoundTag> {

    void put(IFlag flag);

    /**
     * Retrieves the flag associated with the given name.
     *
     * @param flag the name of the flag
     * @return the corresponding {@link IFlag}, or {@code null} if not found
     */
    IFlag get(String flag);

    void remove(String flag);

    /**
     * Checks if a flag with the given name exists in the container.
     *
     * @param flag the name of the flag to check
     * @return {@code true} if the flag exists, otherwise {@code false}
     */
    boolean contains(String flag);

    void clear();

    boolean isEmpty();
    
    int size();
    
    /**
     * Retrieves the state of the specified flag.
     *
     * @param flag the name of the flag to check
     * @return the {@link FlagState} of the flag if it exists; 
     *         otherwise, returns {@link FlagState#UNDEFINED}.
     */
    FlagState flagState(String flag);

    boolean isAllowedOrDenied(@NotNull String flagName);

    /**
     * Checks whether the specified flag is defined (i.e., not {@code UNDEFINED}).
     *
     * @param flag the flag to check, must not be {@code null}
     * @return {@code true} if the flag is defined, otherwise {@code false}
     */
    boolean isFlagDefined(String flag);

    /**
     * Retrieves all flags stored in the container.
     *
     * @return a list of all {@link IFlag} objects in the container
     */
    List<IFlag> flags();

    /**
     * Retrieves all flags that match the specified state.
     *
     * @param state the {@link FlagState} to filter by
     * @return a list of {@link IFlag} objects that have the specified state
     */
    List<IFlag> flags(FlagState state);

    /**
     * Retrieves a set of all flag entries in the container.
     *
     * @return a set of {@link Map.Entry} pairs, where the key is the flag name and the value is the corresponding {@link IFlag}
     */
    Set<Map.Entry<String, IFlag>> flagEntries();
}
