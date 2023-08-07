defmodule DistributedComputingBench.BumblebeeBench.Runner do
  @moduledoc false

  def run() do
    Nx.default_backend(EXLA.Backend)

    {:ok, model_info} = Bumblebee.load_model({:hf, "google/vit-base-patch16-224"})
    {:ok, featurizer} = Bumblebee.load_featurizer({:hf, "google/vit-base-patch16-224"})

    serving =
      Bumblebee.Vision.image_classification(model_info, featurizer,
        compile: [batch_size: 1],
        defn_options: [compiler: EXLA],
        top_k: 1
      )

    Nx.Serving.start_link(name: ViT, serving: serving)
  end
end
