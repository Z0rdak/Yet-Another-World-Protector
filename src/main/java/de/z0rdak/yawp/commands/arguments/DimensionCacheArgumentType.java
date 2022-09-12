package de.z0rdak.yawp.commands.arguments;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.core.Registry;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.Level;

import java.util.Collection;
import java.util.concurrent.CompletableFuture;

public class DimensionCacheArgumentType implements ArgumentType<DimensionRegionCache> {

    private static final Collection<String> EXAMPLES = RegionDataManager.get().getDimensionList();

    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            dim -> new TranslatableComponent("cli.arg.dim.invalid", dim)
    );

    @Override
    public DimensionRegionCache parse(StringReader reader) throws CommandSyntaxException {
        ResourceLocation resourcelocation = ResourceLocation.read(reader);
        return getDimensionalRegionCache(resourcelocation);
    }

    private static DimensionRegionCache getDimensionalRegionCache(ResourceLocation resourcelocation) {
        // TODO: Check valid dimension key?
        ResourceKey<Level> registrykey = ResourceKey.create(Registry.DIMENSION_REGISTRY, resourcelocation);
        if (RegionDataManager.get().containsCacheFor(registrykey)) {
            return RegionDataManager.get().cacheFor(registrykey);
        } else {
            return RegionDataManager.get().newCacheFor(registrykey);
        }
    }

    @Override
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof SharedSuggestionProvider) {
            return SharedSuggestionProvider.suggest(RegionDataManager.get().getDimensionList(), builder);
        } else {
            return Suggestions.empty();
        }
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }

    public static DimensionalRegionArgumentType dimRegion() {
        return new DimensionalRegionArgumentType();
    }

    public static DimensionRegionCache getDimRegion(CommandContext<CommandSourceStack> context, String dim) {
        ResourceLocation resourcelocation = context.getArgument(dim, ResourceLocation.class);
        return getDimensionalRegionCache(resourcelocation);

    }
}
