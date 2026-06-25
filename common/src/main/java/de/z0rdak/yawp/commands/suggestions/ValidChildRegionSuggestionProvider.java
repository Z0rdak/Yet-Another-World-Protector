package de.z0rdak.yawp.commands.suggestions;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.core.region.GlobalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.data.region.LevelRegionData;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.Entity;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

/**
 * Suggests regions that can be assigned as children of the region supplied
 * by a previous command argument.
 *
 * <p>A region is considered a valid child when:</p>
 * <ul>
 *     <li>its area is fully contained within the prospective parent's area</li>
 *     <li>it does not already have a local region as its parent</li>
 *     <li>it resides in the same dimension as the prospective parent</li>
 * </ul>
 *
 * <p>
 * Ownership and permission checks are intentionally not performed by this
 * provider and must be validated during command execution.
 * </p>
 *
 * <p>
 * The provider exists solely to improve command completion and should not
 * be considered an authoritative validation mechanism.
 * </p>
 */
public class ValidChildRegionSuggestionProvider implements SuggestionProvider<CommandSourceStack> {

    @Override
    public CompletableFuture<Suggestions> getSuggestions(CommandContext<CommandSourceStack> context, SuggestionsBuilder builder) {
        IMarkableRegion parent = ArgumentUtil.getLocalRegionArgument(context);
        var maybeLevelData = RegionManager.get().getLevelRegionData(parent.getDim());
        if (maybeLevelData.isEmpty()) {
            return Suggestions.empty();
        }
        LevelRegionData levelData = maybeLevelData.get();
        levelData.getLocalList().stream()
                .filter(candidate -> isValidChild(parent, candidate))
                .forEach(candidate -> builder.suggest(candidate.getName()));
        // TODO: permission check here?
        return builder.buildFuture();
    }

    private static boolean isValidChild(IMarkableRegion parent, IMarkableRegion candidate) {
        return parent.getArea().containsOther(candidate.getArea()) && !candidate.hasLocalParent();
    }
}