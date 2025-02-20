package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.world.entity.player.Player;
import org.jetbrains.annotations.Nullable;

/**
 * Represents the context for evaluating a flag within a protected region.
 *
 * <p>This record stores the region being checked, the specific flag under evaluation, 
 * an optional pre-determined flag value, and the player (if applicable) for permission checks.</p>
 *
 * @param region     The protected region where the flag is being evaluated.
 * @param regionFlag The flag being checked.
 * @param flag       The resolved flag value, if already determined (nullable).
 * @param player     The player for whom the flag is being evaluated (nullable).
 */
public record FlagContext(
        IProtectedRegion region, 
        RegionFlag regionFlag, 
        @Nullable IFlag flag, 
        @Nullable Player player) {

    /**
     * Determines the flag resolution for this context.
     *
     * <p>If the player has bypass permissions in the region, the flag state is set to {@code ALLOWED}.
     * Otherwise, the flag state is determined by the region's flag settings.</p>
     *
     * @return The {@link FlagState} representing the resolved flag and its state.
     */
    public FlagState resultingState() {
        var flagState = region.getFlags().flagState(regionFlag.name);
        return Permissions.playerHasBypassPermission(region, player) 
                ? FlagState.ALLOWED 
                : flagState;
    }

    /**
     * Resolves the effective {@link FlagContext} by considering whether this context should be overridden
     * by a parent context.
     * <p>
     * This method resolves flag inheritance based on the following rules:
     * <ul>
     *   <li>If the parent flag has an override, the parent context is returned.</li>
     *   <li>If the parent flag is set, the child flag is not set, and the player does not have a bypass permission, the parent context is returned.</li>
     *   <li>Otherwise, the current context is retained.</li>
     * </ul>
     * <p> This method is typically used in recursive flag resolution, ensuring that higher-priority 
     * regions or inherited flag settings take effect when applicable.
     *
     * @param parent the parent {@link FlagContext} to inherit from, must not be {@code null}
     * @return the effective {@link FlagContext} after applying inheritance rules
     */
    public FlagContext inheritContext(FlagContext parent) {         
        var playerBypass = Permissions.playerHasBypassPermission(region, player);
        var cFlagSet = region.getFlags().isAllowedOrDenied(regionFlag.name);
        var pFlagSet = parent.region.getFlags().isAllowedOrDenied(regionFlag.name);
        var parentOverrides = parent.flag != null && parent.flag.doesOverride();
        if (!parentOverrides) {
            if (pFlagSet && !cFlagSet && !playerBypass) return parent;
            if (playerBypass || (cFlagSet && pFlagSet)) return this;
            return this;
        } else 
            return parent;
    }

    private FlagContext inheritContext2(FlagContext parent) {
        boolean playerBypass = Permissions.playerHasBypassPermission(region, player);
        boolean childFlagSet = region.getFlags().isAllowedOrDenied(regionFlag.name);
        boolean parentFlagSet = parent.region.getFlags().isAllowedOrDenied(regionFlag.name);
        boolean parentOverrides = parent.flag != null && parent.flag.doesOverride();

        if (parentOverrides || (parentFlagSet && !childFlagSet && !playerBypass)) {
            return parent;
        }
        return this;
    }

    /**
     * Creates a {@link FlagContext} for the parent region of the given region.
     * <p>
     * This method retrieves the parent region of the specified {@code region} and constructs  
     * a {@link FlagContext} using the parent's flag value for the given {@link RegionFlag}.  
     * </p>
     *
     * @param region     the region whose parent context is to be determined, must not be {@code null}
     * @param regionFlag the flag for which the context is being created, must not be {@code null}
     * @param player     the player associated with this flag context, may be {@code null}
     * @return a {@link FlagContext} representing the flag state in the parent region
     */
    public static FlagContext parentOf(IProtectedRegion region, RegionFlag regionFlag, @Nullable Player player) {
        IProtectedRegion parent = region.getParent();
        IFlag flag = parent.getFlag(regionFlag.name);
        return new FlagContext(parent, regionFlag, flag, player);
    }
}
    
